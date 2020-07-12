import { Room, User, RecvMessage, MessageId } from "@/models";
import ApiService from "@/services/api";
import _ from "lodash";
import Vue from "vue";
import Vuex from "vuex";

Vue.use(Vuex);

interface State {
  sessionUserId: string | null;
  users: { [key: string]: User };
  rooms: { [key: string]: Room };
  newMessages: RecvMessage[];
  readMessages: { [key: number]: boolean }; 
}

const initialState: State = {
  sessionUserId: ApiService.sessionUserId,
  users: {},
  rooms: {},
  newMessages: [],
  readMessages: {}
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
    sync(state, { rooms, users, rcvMsgs }: { rooms: Room[]; users: User[], rcvMsgs: RecvMessage[] }) {
      state.rooms = Object.fromEntries(rooms.map(r => [r.id, r]));
      state.users = Object.fromEntries(users.map(u => [u.id, u]));
      state.newMessages = rcvMsgs;
    },
    updateRoom({ rooms }, room: Room) {
      // We assume the room is already in the store. if it's not this won't trigger reactivity
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
    clearMessages(state) {
      state.newMessages = [];
    },
    markRead(state, msgId: MessageId) {
      state.readMessages[msgId] = true;
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
    syncSessionUser: ({ dispatch, state }) => {
      if (state.sessionUserId !== null) {
        return dispatch("syncUser", state.sessionUserId);
      } else {
        return Promise.resolve();
      }
    },
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
    signOut: ({ commit }) =>
      ApiService.signOut().then(() => commit("removeSessionUser")),
    sync: ({ commit }) =>
      Promise.all([
        ApiService.rooms(),
        ApiService.users(),
        ApiService.recvMessages()
      ]).then(r =>
        commit("sync", { rooms: r[0], users: r[1], rcvMsgs: r[2]})
      ),
    selectRoom: ({ commit }, roomId: string) => {
      commit("changeActiveRoom", roomId);
    },
    joinRoom: async ({ commit }, roomId: string) => {
      const zoomLink = await ApiService.joinRoom(roomId);
      commit("swithToRoom", roomId);
      return zoomLink;
    },
    leaveRoom: ({ commit }) =>
      ApiService.leaveRoom().then(() => commit("leaveRoom")),
    recvMessages: ({ commit, state }) => {
      const curMessages = state.newMessages.filter(m => !state.readMessages[m.messageId]);
      commit("clearMessages");
      return curMessages;
    },
    markRead: ({ commit }, msgId: MessageId) => {
      commit("markRead", msgId);
      ApiService.markRead(msgId);
    },
    isRead: ({ state}, msgId: MessageId) => state.readMessages[msgId] 
  },
  getters: {
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
    roomUsers: ({ users }) => (roomId: string) =>
      _.filter(_.values(users), u => u.room == roomId),
    room: ({ rooms }, getters) => (roomId: string) =>
      rooms[roomId] && addUsersToRoom(rooms[roomId], getters.roomUsers(roomId)),
    currentRoom: (_state, getters) => {
      const currentRoomId = getters.sessionUser?.room;
      return currentRoomId && getters.room(currentRoomId);
    },
    userById: ({ users }) => (userId: string) => 
      users[userId]
  },
  modules: {},
  strict: process.env.NODE_ENV !== "production"
});
