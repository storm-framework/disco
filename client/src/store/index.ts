import Vue from "vue";
import Vuex from "vuex";
import ApiService from "@/services/api";
import { Entity, Room, User } from "@/models";

Vue.use(Vuex);

interface State {
  loading: boolean;
  users: Map<string, User>;
  rooms: Map<string, any>;
  activeRoomId: string | null;
}

const initialState: State = {
  loading: false,
  users: new Map(),
  rooms: new Map(),
  activeRoomId: null
};

export default new Vuex.Store({
  state: initialState,
  mutations: {
    requestStart(state) {
      state.loading = true;
    },
    initHome(
      state,
      { rooms, users }: { rooms: Entity<Room>[]; users: User[] }
    ) {
      state.loading = false;
      state.rooms = new Map(rooms.map(r => [r.id, r]));
      state.users = new Map(users.map(u => [u.id, u]));
    },
    changeActiveRoom(state, roomId: string) {
      state.activeRoomId = roomId;
    },
    updateRoom(state, { roomId, room }) {
      state.rooms.set(roomId, room);
    }
  },
  actions: {
    initHome({ commit }) {
      commit("requestStart");
      Promise.all([ApiService.rooms(), ApiService.users()])
        .then(r => commit("initHome", { rooms: r[0], users: r[1] }))
        .catch(console.log);
    },
    selectRoom({ commit }, roomId: string) {
      commit("changeActiveRoom", roomId);
    },
    joinRoom({ commit, state }) {
      const roomId = state.activeRoomId;
      if (!roomId) {
        return;
      }
      commit("requestStart");
      ApiService.joinRoom(roomId)
        .then(room => commit("updateRoom", { roomId, room }))
        .catch(console.log);
    }
  },
  getters: {
    roomsWithUsers: ({ rooms, users }) => {
      const result = new Map(rooms.entries());
      for (const u of users.values()) {
        const r = u.room && result.get(u.room);
        if (r) {
          r.data.users = r?.data.users || [];
          r.data.users.push(u);
        }
      }
      return Array.from(result.values());
    },
    activeRoom: ({ rooms, activeRoomId }) =>
      activeRoomId && rooms.get(activeRoomId)
  },
  modules: {},
  strict: process.env.NODE_ENV !== "production"
});
