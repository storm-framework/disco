<template>
  <b-form class="form-send-invitations text-center" @submit="onSubmit">
    <b-overlay
      :show="loading"
      spinner-variant="primary"
      spinner-type="grow"
      spinner-small
      rounded="sm"
      bg-color="#f5f5f5"
    >
      <b-container>
        <b-alert :show="fatalError" variant="danger">{{ errorMsg }}</b-alert>
        <b-row class="mb-3">
          <b-col sm>
            <span class="font-weight-bold">Name</span>
          </b-col>
          <b-col sm>
            <span class="font-weight-bold">Capacity</span>
          </b-col>
          <b-col sm>
            <span class="font-weight-bold">URL</span>
          </b-col>
        </b-row>

        <b-row
          v-for="item in oldRooms"
          v-bind:key="'old:' + item.id"
          class="mb-3"
        >
          <b-col sm>
            <b-form-input
              type="text"
              v-model="item.name"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
          <b-col sm>
            <b-form-input
              type="number"
              v-model="item.capacity"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
          <b-col sm>
            <b-form-input
              type="url"
              v-model="item.zoomLink"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
        </b-row>

        <hr />

        <b-row
          v-for="(item, index) in newRooms"
          v-bind:key="'new:' + index"
          class="mb-3"
        >
          <b-col sm>
            <b-form-input
              type="text"
              v-model="item.name"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
          <b-col sm>
            <b-form-input
              type="number"
              v-model="item.capacity"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
          <b-col sm>
            <b-form-input
              type="url"
              v-model="item.zoomLink"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
        </b-row>
        <div clas="d-flex">
          <b-button
            :disabled="fatalError"
            size="lg"
            variant="light"
            class="btn-plus"
            v-on:click="add"
          >
            <b-icon-plus-circle />
          </b-button>
        </div>

        <b-button variant="primary" size="lg" type="submit" class="mt-4">
          Save
        </b-button>
      </b-container>
    </b-overlay>
  </b-form>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { Room, RoomEntity } from "../models";
import ApiService from "../services/api";

interface OldRow {
  id: string;
  name: string;
  capacity: string;
  zoomLink: string;
}

interface NewRow {
  name: string;
  capacity: string;
  zoomLink: string;
}

function parseNewRow(row: NewRow): Room {
  return {
    name: row.name,
    capacity: parseInt(row.capacity) || 0,
    zoomLink: row.zoomLink
  };
}

function parseOldRow(row: OldRow): RoomEntity {
  return {
    id: row.id,
    room: parseNewRow(row)
  };
}

@Component
export default class SignIn extends Vue {
  loading = false;
  oldRooms: OldRow[] = [];
  newRooms: NewRow[] = [];
  fatalError = false;
  errorMsg = "";

  mounted() {
    this.loading = true;
    ApiService.rooms()
      .then(rooms => {
        this.loading = false;
        this.oldRooms = rooms.map(r => {
          return {
            id: r.id,
            name: r.room.name,
            capacity: r.room.capacity.toString(),
            zoomLink: r.room.zoomLink
          };
        });
      })
      .catch(() => {
        this.loading = false;
        this.setFatalError("Internal server error");
      });
  }

  add() {
    this.newRooms.push({ name: "Room #1", capacity: "10", zoomLink: "" });
  }

  onSubmit(evt: Event) {
    const updates = this.oldRooms.map(parseOldRow);
    const inserts = this.newRooms.map(parseNewRow);
    this.loading = true;
    ApiService.updateRooms(updates, inserts)
      .then(ids => {
        for (const i in this.newRooms) {
          this.oldRooms.push({ id: ids[i], ...this.newRooms[i] });
        }
        this.newRooms = [];
        this.loading = false;
      })
      .catch(() => {
        this.loading = false;
      });
    evt.preventDefault();
  }

  setFatalError(msg: string) {
    this.errorMsg = msg;
    this.fatalError = true;
  }
}
</script>

<style lang="scss">
.btn-plus {
  background-color: transparent;
  border-color: transparent;
}
</style>
