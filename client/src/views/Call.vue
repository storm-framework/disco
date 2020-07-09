<template>
  <main>
    <jitsi-call
      v-if="currentRoom"
      class="call"
      :room-name="currentRoom.zoomLink"
      :display-name="sessionUser.displayName"
      :avatar-url="sessionUser.photoUrl || ''"
    />
  </main>
</template>

<script lang="ts">
import { Component, Vue } from "vue-property-decorator";
import JitsiCall from "@/components/JitsiCall.vue";
import { mapGetters } from "vuex";

@Component({
  components: { JitsiCall },
  computed: mapGetters(["sessionUser", "currentRoom"])
})
export default class Call extends Vue {}
</script>

<style>
.call {
  width: 100vw;
  height: 100vh;
}
</style>
