<template>
  <b-modal
    :title="modalTitle"
    :id="modalId"
    :header-bg-variant="'info'"
    :header-text-variant="'light'"
    :body-bg-variant="'light'"
    :body-text-variant="'dark'"
    :footer-bg-variant="'dark'"
    :footer-text-variant="'light'"
    :ok-title="'Send'"
    @ok="send"
    @cancel="clear"
    @keydown.native.enter="send"
    hide-header-close
    no-close-on-esc
    no-close-on-backdrop
  >
    <form>
      <div>
        <b-form-input v-model="message" placeholder="Hello!"></b-form-input>
      </div>
    </form>
  </b-modal>
</template>

<script lang="ts">
import { Component, Prop, Vue } from "vue-property-decorator";

import ApiService from "@/services/api";
import { User } from "@/models";

@Component({})
export default class SendMessage extends Vue {
  @Prop({ default: "message-modal" })
  modalId!: string;

  @Prop({ default: "Send Message" })
  modalTitle!: string;

  @Prop({ default: null })
  receiver!: number | null;

  message = "";

  name = "send-message";

  send() {
    const sender: User = this.$store.getters.sessionUser;
    if (sender) {
      ApiService.sendMessage({
        senderId: sender.id,
        receiverId: this.receiver,
        messageText: this.message,
        timestamp: new Date().getTime()
      }).then(() => this.clear());
    }
  }

  clear() {
    this.message = "";
  }
}
</script>
