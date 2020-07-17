<template>
  <b-modal
    :title="title"
    :id="modalId"
    header-bg-variant="primary"
    header-text-variant="light"
    body-bg-variant="light"
    body-text-variant="dark"
    ok-title="Send"
    cancel-variant="outline-secondary"
    @ok.prevent="send"
    @hidden="clear"
    no-close-on-esc
    :ok-disabled="sending"
    :no-close-on-backdrop="sending"
    :cancel-disabled="sending"
  >
    <form>
      <div>
        <b-alert :show="error" variant="danger" dismissible>
          {{ errorMsg }}
        </b-alert>
        <b-form-textarea v-model="message" placeholder="Hello!" />
      </div>
    </form>
  </b-modal>
</template>

<script lang="ts">
import { Component, Prop, Vue } from "vue-property-decorator";

import ApiService from "@/services/api";
import { User } from "@/models";
import { BvModalEvent, BModal } from "bootstrap-vue";

@Component({})
export default class SendMessage extends Vue {
  @Prop()
  modalId?: string;

  @Prop({ default: null })
  receiver!: number | null;

  sending = false;
  message = "";
  errorMsg = "";
  error = false;

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

  send(ev: BvModalEvent) {
    if (this.sending) {
      return;
    }
    const sender: User = this.$store.getters.sessionUser;
    if (sender) {
      this.sending = true;
      ApiService.sendMessage({
        senderId: sender.id,
        receiverId: this.receiver,
        messageText: this.message,
        timestamp: new Date().getTime()
      })
        .then(() => (ev.vueTarget as BModal).hide("message-sent"))
        .catch(() => this.setError("There was an unexpected error."))
        .finally(() => (this.sending = false));
    } else {
      this.setError("There was an unexpected error.");
    }
  }

  clear() {
    this.message = "";
    this.error = false;
  }

  setError(msg: string) {
    this.error = true;
    this.errorMsg = msg;
  }
}
</script>
