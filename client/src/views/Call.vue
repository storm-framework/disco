<template>
  <main>
    <jitsi-call
      class="call"
      room-name="avsbtlnwvcdmvjerwhbkjalkasdncb"
      display-name="Foo Barlington"
    />
  </main>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import JitsiCall from "@/components/JitsiCall.vue";

@Component({ components: { JitsiCall } })
export default class Call extends Vue {}
</script>

<style>
.call {
  width: 800px;
  height: 450px;
  margin: 10px auto;
}
</style>
