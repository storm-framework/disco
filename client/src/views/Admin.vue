<template>
  <main class="mt-4">
    <h1>Manage rooms</h1>
    <b-form class="form-send-invitations" @submit.prevent="onSubmit">
      <b-alert :show="fatalError" variant="danger">{{ errorMsg }}</b-alert>
      <fieldset v-if="!loading" :disabled="fatalError">
        <table class="rooms-table">
          <thead>
            <tr>
              <th class="room-color sr-only">Color</th>
              <th class="room-name">Name</th>
              <th class="room-topic">Topic</th>
              <th class="room-capacity">Capacity</th>
              <th class="room-url">URL</th>
            </tr>
          </thead>
          <tbody v-for="group in roomGroups" :key="group">
            <tr
              v-for="(room, index) in groupRooms(group)"
              :key="keyFor(group, index)"
            >
              <td class="room-color">
                <b-form-input type="color" v-model="room.color" />
              </td>
              <td class="room-name">
                <b-form-input type="text" v-model="room.name" required />
              </td>
              <td class="room-topic">
                <b-form-input type="text" v-model="room.topic" />
              </td>
              <td class="room-capacity">
                <b-form-input type="number" v-model="room.capacity" />
              </td>
              <td class="room-url">
                <b-form-input type="url" v-model="room.zoomLink" required />
              </td>
            </tr>
            <tr v-if="group === 'new'">
              <td colspan="3" class="new-row">
                <icon-button icon="plus" class="add-room" @click="add">
                  Add Room
                </icon-button>
              </td>
            </tr>
          </tbody>
        </table>
        <b-button
          type="submit"
          variant="primary"
          class="save"
          :disabled="saving"
        >
          Save
        </b-button>
      </fieldset>
    </b-form>
  </main>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { Room, RoomData } from "@/models";
import ApiService from "@/services/api";
import NTC from "@/vendor/ntc";

import { library } from "@fortawesome/fontawesome-svg-core";
import { faPlus } from "@fortawesome/free-solid-svg-icons";
library.add(faPlus);

interface OldRoom {
  id: number;
  name: string;
  zoomLink: string;
  color: string;
  capacity: string;
  topic: string;
}

interface NewRoom {
  name: string;
  zoomLink: string;
  color: string;
  capacity: string;
  topic: string;
}

function parseNewRoom(row: NewRoom): RoomData {
  return {
    name: row.name,
    capacity: parseInt(row.capacity),
    zoomLink: row.zoomLink,
    color: row.color,
    topic: row.topic
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
  return `https://meet.jit.si/${r}`;
}

type AnyRoom = OldRoom | NewRoom;
type RoomGroup = "old" | "new";

@Component
export default class SignIn extends Vue {
  loading = false;
  saving = false;
  oldRooms: OldRoom[] = [];
  newRooms: NewRoom[] = [];
  fatalError = false;
  errorMsg = "";
  readonly roomGroups = ["old", "new"];

  groupRooms(group: RoomGroup): AnyRoom[] {
    const groups = {
      old: this.oldRooms,
      new: this.newRooms
    };
    return groups[group];
  }

  keyFor(group: RoomGroup, index: number) {
    let id: number;
    if (group === "old") {
      id = this.oldRooms[index].id;
    } else {
      id = index;
    }
    return `${group}:${id}`;
  }

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
            topic: r.topic,
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
    const n = this.oldRooms.length + this.newRooms.length;
    // Using the golden ratio means that colors get evenly spread out and never overlap.
    const hue = (n * GOLDEN_RATIO) % 1;
    const [r, g, b] = hslToRgb(hue, 0.8, 0.6);
    const color = rgbToHex(r, g, b);
    const name = NTC.name(color)[1];
    this.newRooms.push({
      name: `${name} Room`,
      zoomLink: randomJitsiLink(),
      topic: "",
      capacity: "0",
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
      })
      .finally(() => {
        this.saving = false;
      });
  }

  setFatalError(msg: string) {
    this.errorMsg = msg;
    this.fatalError = true;
  }
}
</script>

<style lang="scss" scoped>
@import "~bootstrap/scss/bootstrap";

main {
  @include make-container-max-widths();
  margin-left: auto;
  margin-right: auto;
}

.rooms-table {
  &,
  thead,
  tbody,
  tfoot,
  tr,
  th,
  td {
    display: block;
  }

  tr {
    @include make-row();
    margin-bottom: $spacer;
  }

  th,
  td {
    @include make-col-ready();
  }

  th {
    font-size: 1.25em;
  }
}

.new-row {
  text-align: center;
  width: 100%;
}

.room-color {
  @include make-col(1);
}

.room-name {
  @include make-col(3);
}

.room-topic {
  @include make-col(4);
}

.room-capacity {
  @include make-col(1);
}

.room-url {
  @include make-col(3);
}

.save {
  float: right;
}
</style>
