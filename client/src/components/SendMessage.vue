<template>
  <b-modal
    :title="title"
    :id="modalId"
    :header-bg-variant="'primary'"
    :header-text-variant="'light'"
    :body-bg-variant="'light'"
    :body-text-variant="'dark'"
    :ok-title="'Send'"
    cancel-variant="outline-secondary"
    @ok="send"
    @cancel="clear"
    @keydown.native.enter="send"
    hide-header-close
    no-close-on-esc
    no-close-on-backdrop
  >
    <form>
      <div>
        <b-form-textarea v-model="message" placeholder="Hello!" />
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
  @Prop()
  modalId?: string;

  @Prop({ default: null })
  receiver!: number | null;

  message = "";

  get title() {
    let receiverName: string | null;
    if (this.receiver) {
      receiverName = this.$store.getters.userById(this.receiver)?.displayName;
    } else {
      receiverName = "everyone";
    }
    if (receiverName) {
      return "Send a message to " + receiverName;
    } else {
      return "Send a message";
    }
  }

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
