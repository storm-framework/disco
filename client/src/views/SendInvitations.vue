<template>
  <div>
    <navbar />
    <b-form class="form-send-invitations text-center" @submit="onSubmit">
      <b-container>
        <b-row class="mb-3">
          <b-col sm>
            <span class="font-weight-bold">Email address</span>
          </b-col>
          <b-col sm>
            <span class="font-weight-bold">Full name</span>
          </b-col>
        </b-row>
        <b-row v-for="(item, index) in rows" v-bind:key="index" class="mb-3">
          <b-col sm>
            <b-form-input
              type="email"
              v-model="item.emailAddress"
              required
            ></b-form-input>
          </b-col>
          <b-col sm>
            <b-form-input
              type="text"
              v-model="item.fullName"
              required
            ></b-form-input>
          </b-col>
        </b-row>
        <div clas="d-flex">
          <b-button size="lg" variant="light" class="btn-plus" v-on:click="add">
            <b-icon-plus-circle />
          </b-button>
        </div>
        <b-button variant="primary" size="lg" type="submit" class="mt-4">
          Send
        </b-button>
      </b-container>
    </b-form>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import { Invitation } from "@/models";
import ApiService from "@/services/api";
import Navbar from "@/components/Navbar.vue";

@Component({ components: { Navbar } })
export default class SignIn extends Vue {
  rows: Invitation[] = [{ emailAddress: "", fullName: "" }];

  add() {
    this.rows.push({ emailAddress: "", fullName: "" });
  }

  onSubmit(evt: Event) {
    ApiService.sendInvitations(this.rows).then(console.log);
    evt.preventDefault();
  }
}
</script>

<style lang="scss">
.btn-plus {
  background-color: transparent;
  border-color: transparent;
}
</style>
