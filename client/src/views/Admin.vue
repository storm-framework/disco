<template>
  <div class="admin">
    <b-form
      class="form-send-invitations text-center mt-5"
      @submit.prevent="onSubmit"
    >
      <b-container v-if="!loading">
        <b-alert :show="fatalError" variant="danger">{{ errorMsg }}</b-alert>
        <b-row class="mb-3">
          <b-col sm>
            <span class="font-weight-bold">Name</span>
          </b-col>
          <!-- <b-col sm>
            <span class="font-weight-bold">Capacity</span>
          </b-col> -->
          <b-col sm>
            <span class="font-weight-bold">URL</span>
          </b-col>
        </b-row>

        <b-row
          v-for="item in oldRooms"
          v-bind:key="'old:' + item.id"
          class="mb-3"
        >
          <b-col sm="1">
            <b-form-input
              type="color"
              v-model="item.color"
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
          <b-col sm>
            <b-form-input
              type="text"
              v-model="item.name"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
          <!-- <b-col sm>
            <b-form-input
              type="number"
              v-model="item.capacity"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col> -->
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
          <b-col sm="1">
            <b-form-input
              type="color"
              v-model="item.color"
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
          <b-col sm>
            <b-form-input
              type="text"
              v-model="item.name"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col>
          <!-- <b-col sm>
            <b-form-input
              type="number"
              v-model="item.capacity"
              required
              :disabled="fatalError"
            ></b-form-input>
          </b-col> -->
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
import NTC from "@/vendor/ntc";

interface OldRoom {
  id: number;
  name: string;
  zoomLink: string;
  color: string;
}

interface NewRoom {
  name: string;
  zoomLink: string;
  color: string;
}

function parseNewRoom(row: NewRoom): RoomInsert {
  return {
    name: row.name,
    capacity: 10,
    zoomLink: row.zoomLink,
    color: row.color
  };
}

function parseOldRoom(row: OldRoom): Room {
  return {
    id: row.id,
    ...parseNewRoom(row)
  };
}

const GOLDEN_RATIO: number = (1 + Math.sqrt(5)) / 2;

function hue2rgb(p: number, q: number, t: number) {
  if (t < 0) t += 1;
  if (t > 1) t -= 1;
  if (t < 1 / 6) return p + (q - p) * 6 * t;
  if (t < 1 / 2) return q;
  if (t < 2 / 3) return p + (q - p) * (2 / 3 - t) * 6;
  return p;
}
function hslToRgb(h: number, s: number, l: number) {
  let r, g, b;

  if (s == 0) {
    r = g = b = l; // achromatic
  } else {
    const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
    const p = 2 * l - q;

    r = hue2rgb(p, q, h + 1 / 3);
    g = hue2rgb(p, q, h);
    b = hue2rgb(p, q, h - 1 / 3);
  }

  return [r * 255, g * 255, b * 255];
}

function rgbToHex(r: number, g: number, b: number) {
  r = Math.floor(r);
  g = Math.floor(g);
  b = Math.floor(b);
  return `#${r.toString(16)}${g.toString(16)}${b.toString(16)}`;
}

function randomString() {
  return Math.random()
    .toString(36)
    .substring(2, 15);
}

function randomJitsiLink() {
  const r = randomString() + randomString();
  return `https://meet.jitsi.si/${r}`;
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
    const n = this.oldRooms.length + this.newRooms.length;
    const hue = (n * GOLDEN_RATIO) % 1;
    const [r, g, b] = hslToRgb(hue, 0.8, 0.6);
    const color = rgbToHex(r, g, b);
    const name = NTC.name(color)[1];
    this.newRooms.push({
      name: `${name} Room`,
      zoomLink: randomJitsiLink(),
      color: color
    });
  }

  onSubmit() {
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
