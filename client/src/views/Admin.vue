<template>
  <div class="admin">
    <b-form class="form-send-invitations text-center mt-5" @submit="onSubmit">
      <b-container v-if="!loading">
        <b-alert :show="fatalError" variant="danger">{{ errorMsg }}</b-alert>
        <b-row class="mb-3">
          <b-col sm>
            <span class="font-weight-bold">Name</span>
          </b-col>
          <b-col sm>
            <span class="font-weight-bold">Color</span>
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
              type="text"
              v-model="item.color"
              required
              :disabled="fatalError"
            />
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
              type="text"
              v-model="item.color"
              required
              :disabled="fatalError"
            />
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

        <b-button
          variant="primary"
          size="lg"
          type="submit"
          :disabled="saving"
          class="mt-3"
        >
          Save
        </b-button>
      </b-container>
    </b-form>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { Room, RoomInsert } from "@/models";
import ApiService from "@/services/api";

interface OldRoom {
  id: string;
  name: string;
  color: string;
  capacity: string;
  zoomLink: string;
}

interface NewRoom {
  name: string;
  color: string;
  capacity: string;
  zoomLink: string;
}

function parseNewRoom(row: NewRoom): RoomInsert {
  return {
    name: row.name,
    color: row.color,
    capacity: parseInt(row.capacity) || 0,
    zoomLink: row.zoomLink
  };
}

function parseOldRoom(row: OldRoom): Room {
  return {
    id: row.id,
    ...parseNewRoom(row)
  };
}

@Component
export default class SignIn extends Vue {
  loading = false;
  saving = false;
  oldRooms: OldRoom[] = [];
  newRooms: NewRoom[] = [];
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
            name: r.name,
            color: r.color,
            capacity: r.capacity.toString(),
            zoomLink: r.zoomLink
          };
        });
      })
      .catch(() => {
        this.loading = false;
        this.setFatalError("Internal server error");
      });
  }

  add() {
    this.newRooms.push({
      name: "Room #1",
      color: "red",
      capacity: "10",
      zoomLink: ""
    });
  }

  onSubmit(evt: Event) {
    if (this.saving) {
      return;
    }
    const updates = this.oldRooms.map(parseOldRoom);
    const inserts = this.newRooms.map(parseNewRoom);
    this.saving = true;
    ApiService.updateRooms(updates, inserts)
      .then(ids => {
        for (const i in this.newRooms) {
          this.oldRooms.push({ id: ids[i], ...this.newRooms[i] });
        }
        this.newRooms = [];
        this.saving = false;
      })
      .catch(() => {
        this.saving = false;
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
