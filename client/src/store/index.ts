import { MessageId, RecvMessage, Room, User } from "@/models";
import ApiService from "@/services/api";
import _ from "lodash";
import Vue from "vue";
import Vuex from "vuex";

Vue.use(Vuex);

interface State {
  sessionUserId: number | null;
  users: { [key: string]: User };
  rooms: { [key: string]: Room };
  receivedMessages: { [key: number]: boolean };
}

const initialState: State = {
  sessionUserId: null,
  users: {},
  rooms: {},
  receivedMessages: {}
};

function addUsersToRoom(room: Room, users: User[]) {
  return {
    users,
    ...room
  };
}

export default new Vuex.Store({
  state: initialState,
  mutations: {
    sync(state, { rooms, users }: { rooms: Room[]; users: User[] }) {
      state.rooms = Object.fromEntries(rooms.map(r => [r.id, r]));
      state.users = Object.fromEntries(users.map(u => [u.id, u]));
    },
    updateRoom({ rooms }, room: Room) {
      // We assume the room is already in the store. if it isn't this won't trigger reactivity
      rooms[room.id] = room;
    },
    swithToRoom(state, roomId) {
      const u = state.sessionUserId && state.users[state.sessionUserId];
      if (u) {
        u.room = roomId;
      }
    },
    updateUser(state, user) {
      Vue.set(state.users, user.id, user);
    },
    setSessionUser(state, userId) {
      state.sessionUserId = userId;
    },
    removeSessionUser(state) {
      state.sessionUserId = null;
    },
    leaveRoom({ users, sessionUserId }) {
      if (sessionUserId) {
        users[sessionUserId].room = null;
      }
    },
    markAsReceived(state, messages: RecvMessage[]) {
      for (const m of messages) {
        Vue.set(state.receivedMessages, m.messageId, true);
      }
    }
  },
  actions: {
    signIn: ({ commit }, { emailAddress, password }) =>
      ApiService.signIn(emailAddress, password).then(user => {
        commit("updateUser", user);
        commit("setSessionUser", user.id);
      }),
    signUp: ({ commit }, data) =>
      ApiService.signUp(data).then(user => {
        commit("updateUser", user);
        commit("setSessionUser", user.id);
      }),
    syncSessionUser: ({ commit }) =>
      ApiService.user("me").then(user => {
        commit("updateUser", user);
        commit("setSessionUser", user.id);
      }),
    syncUser: ({ commit }, userId: number) =>
      ApiService.user(userId).then(user => commit("updateUser", user)),
    updateUserDataMe: ({ commit }, data) =>
      ApiService.updateUserDataMe(data).then(user =>
        commit("updateUser", user)
      ),
    updateRoom: ({ commit }, { roomId, data }) =>
      ApiService.updateRoom(roomId, data).then(room =>
        commit("updateRoom", room)
      ),
    updateTopic: ({ commit }, { roomId, topic }) =>
      ApiService.updateTopic(roomId, topic).then(room =>
        commit("updateRoom", room)
      ),
    signOut: ({ commit }) =>
      ApiService.signOut().then(() => commit("removeSessionUser")),
    sync: ({ state, commit }) =>
      Promise.all([
        ApiService.rooms(),
        ApiService.users(),
        ApiService.recvMessages()
      ]).then(r => {
        const newMessages = _.filter(
          r[2],
          m => !state.receivedMessages[m.messageId]
        );
        commit("sync", { rooms: r[0], users: r[1] });
        commit("markAsReceived", r[2]);
        return newMessages;
      }),
    markAsRead: (_, messageId: MessageId) => ApiService.markRead(messageId),
    selectRoom: ({ commit }, roomId: string) => {
      commit("changeActiveRoom", roomId);
    },
    joinRoom: async ({ commit }, roomId: string) => {
      const zoomLink = await ApiService.joinRoom(roomId);
      commit("swithToRoom", roomId);
      return zoomLink;
    },
    leaveRoom: ({ commit }) =>
      ApiService.leaveRoom().then(() => commit("leaveRoom"))
  },
  getters: {
    allowDirectMessages: () => true,
    loggedIn: ({ sessionUserId }) => !!sessionUserId,
    sessionUser: ({ users, sessionUserId }) =>
      sessionUserId && users[sessionUserId],
    rooms: ({ rooms }) => Object.values(rooms),
    availableRooms: (_state, getters) => {
      if (getters.currentRoom) {
        return _.filter(
          getters.rooms,
          room => room.id !== getters.currentRoom.id
        );
      } else {
        return getters.rooms;
      }
    },
    waitingRooms: (_state, getters) =>
      _.filter(getters.rooms, room => !getters.isVideoRoom(room)),
    roomUsers: ({ users }) => (roomId: string) =>
      _.filter(_.values(users), u => u.room == roomId),
    room: ({ rooms }, getters) => (roomId: string) =>
      rooms[roomId] && addUsersToRoom(rooms[roomId], getters.roomUsers(roomId)),
    currentRoom: (_state, getters) => {
      const currentRoomId = getters.sessionUser?.room;
      return currentRoomId && getters.room(currentRoomId);
    },
    isVideoRoom: () => (room: Room) =>
      room && !room.zoomLink.includes("waiting-room"),
    lobbyUsers: ({ users }) => _.filter(_.values(users), u => u.room == null),
    userById: ({ users }) => (userId: string) => users[userId]
  },
  modules: {},
  strict: process.env.NODE_ENV !== "production"
});
