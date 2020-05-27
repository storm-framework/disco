import Vue from "vue";
import Vuex from "vuex";
import ApiService from "@/services/api";
import { Entity, Room, User } from "@/models";
import _ from "lodash";

Vue.use(Vuex);

interface State {
  sessionUserId: string | null;
  loading: boolean;
  users: { [key: string]: User };
  rooms: { [key: string]: Entity<Room> };
  activeRoomId: string | null;
}

const initialState: State = {
  sessionUserId: ApiService.sessionUserId,
  loading: false,
  users: {},
  rooms: {},
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
    setSessionUser(state, user) {
      state.sessionUserId = user.id;
    }
  },
  actions: {
    signIn: ({ commit }, { emailAddress, password }) =>
      ApiService.signIn(emailAddress, password).then(user => {
        commit("setSessionUser", user);
      }),
    initHome: ({ commit }) => {
      commit("requestStart");
      Promise.all([ApiService.rooms(), ApiService.users()]).then(r =>
        commit("initHome", { rooms: r[0], users: r[1] })
      );
    },
    selectRoom: ({ commit }, roomId: string) => {
      commit("changeActiveRoom", roomId);
    },
    joinRoom: ({ commit, state }) => {
      const roomId = state.activeRoomId;
      if (!roomId) {
        return;
      }
      commit("requestStart");
      return ApiService.joinRoom(roomId).then(zoomLink => {
        commit("swithToRoom", roomId);
        return zoomLink;
      });
    }
  },
  getters: {
    roomsWithUsers: ({ rooms, users }) => {
      const usersByRoom = _(users)
        .values()
        .groupBy(u => u.room)
        .value();
      return _({})
        .assign(rooms)
        .assignWith(usersByRoom, (room, users) => {
          return {
            id: room.id,
            data: {
              users,
              ...room.data
            }
          };
        })
        .values()
        .value();
    },
    activeRoom: ({ rooms, activeRoomId, users }) => {
      if (activeRoomId && rooms[activeRoomId]) {
        const roomUsers = _(users)
          .values()
          .filter(u => u.room == activeRoomId)
          .value();
        return {
          id: activeRoomId,
          data: {
            users: roomUsers,
            ...rooms[activeRoomId].data
          }
        };
      } else {
        return null;
      }
    }
  },
  modules: {},
  strict: process.env.NODE_ENV !== "production"
});
