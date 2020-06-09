import { Room, User } from "@/models";
import ApiService from "@/services/api";
import _ from "lodash";
import Vue from "vue";
import Vuex from "vuex";

Vue.use(Vuex);

interface State {
  sessionUserId: string | null;
  users: { [key: string]: User };
  rooms: { [key: string]: Room };
  activeRoomId: string | null;
}

const initialState: State = {
  sessionUserId: ApiService.sessionUserId,
  users: {},
  rooms: {},
  activeRoomId: null
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
    changeActiveRoom(state, roomId: string) {
      state.activeRoomId = roomId;
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
        dispatch("syncUser", state.sessionUserId);
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
    signOut: ({ commit }) =>
      ApiService.signOut().then(() => commit("removeSessionUser")),
    sync: ({ commit }) =>
      Promise.all([ApiService.rooms(), ApiService.users()]).then(r =>
        commit("sync", { rooms: r[0], users: r[1] })
      ),
    selectRoom: ({ commit }, roomId: string) => {
      commit("changeActiveRoom", roomId);
    },
    joinRoom: async ({ commit }, roomId: string) => {
      if (!roomId) {
        return Promise.reject();
      }
      const zoomLink = await ApiService.joinRoom(roomId);
      commit("swithToRoom", roomId);
      return zoomLink;
    },
    leaveRoom: ({ commit }) =>
      ApiService.leaveRoom().then(() => commit("leaveRoom"))
  },
  getters: {
    loggedIn: ({ sessionUserId }) => !!sessionUserId,
    sessionUser: ({ users, sessionUserId }) =>
      sessionUserId && users[sessionUserId],
    rooms: ({ rooms, users }) => {
      const usersByRoom = _(users)
        .values()
        .filter(u => u.room !== null)
        .groupBy(u => u.room)
        .value();
      return _({})
        .assign(rooms)
        .assignWith(usersByRoom, addUsersToRoom)
        .values()
        .value();
    },
    roomUsers: ({ users }) => (roomId: string) =>
      _(users)
        .values()
        .filter(u => u.room == roomId)
        .value(),
    room: ({ rooms }, getters) => (roomId: string) =>
      rooms[roomId] && addUsersToRoom(rooms[roomId], getters.roomUsers(roomId)),
    activeRoom: ({ activeRoomId }, getters) =>
      activeRoomId && getters.room(activeRoomId),
    currentRoom: (_state, getters) => {
      const currentRoomId = getters.sessionUser?.room;
      return currentRoomId && getters.room(currentRoomId);
    }
  },
  modules: {},
  strict: process.env.NODE_ENV !== "production"
});
