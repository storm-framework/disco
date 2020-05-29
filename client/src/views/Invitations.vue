<template>
  <div class="invitations">
    <navbar />
    <b-container>
      <hot-table
        ref="table"
        :data="invitations"
        :settings="hotSettings"
        :height="windowHeight - 150"
      >
        <!-- <hot-column title="Email address" validator="email-address"></hot-column>
      <hot-column title="Full name"></hot-column> -->
      </hot-table>
    </b-container>
  </div>
</template>

<script lang="ts">
import { HotTable, HotColumn } from "@handsontable/vue";
import Navbar from "@/components/Navbar.vue";
import { Component, Vue } from "vue-property-decorator";
import ApiService from "@/services/api";
import "handsontable/dist/handsontable.full.css";
import { vueWindowSizeMixin } from "vue-window-size";
import Handsontable from "handsontable";

@Component({
  components: { Navbar, HotTable, HotColumn },
  mixins: [vueWindowSizeMixin]
})
export default class Invitations extends Vue {
  hotSettings = {
    width: "100%",
    licenseKey: "non-commercial-and-evaluation",
    stretchH: "all",
    colHeaders: ["Email address", "Full name", "Accepted"],
    rowHeaders: true,
    readOnly: true,
    columns: [
      {},
      {},
      {
        renderer: function(
          instance: Handsontable,
          td: HTMLTableCellElement,
          row: any,
          col: any,
          prop: any,
          value: boolean
        ) {
          console.log(value);
          Handsontable.dom.empty(td);
          if (value) {
            const icon = document.createElement("i");
            icon.className = "fa fa-check-square";

            td.appendChild(icon);
          }
          td.className += " htCenter";

          return td;
        }
      }
    ]
  };
  invitations: [string, string, boolean][] = [];

  mounted() {
    ApiService.getInvitations().then(d => {
      this.invitations = d.map(r => [r.emailAddress, r.fullName, r.accepted]);
    });
  }
}
</script>
