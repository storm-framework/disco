<template>
  <div class="send-invitations">
    <b-container>
      <b-row class="justify-content-end align-items-center mb-3">
        <b-col>
          <span class="invalid-data" v-show="hasErrors">
            The data contains erros
          </span>
        </b-col>
        <b-col cols="auto">
          <b-button
            size="sm"
            class="mr-1"
            v-b-modal.import-file-modal
            variant="outline-primary"
          >
            <font-awesome-icon icon="file-import" /> Import
          </b-button>
          <b-button size="sm" v-on:click="onSend" variant="primary">
            <font-awesome-icon icon="paper-plane" /> Send
          </b-button>
        </b-col>
      </b-row>
      <hot-table
        ref="table"
        :data="rows"
        :settings="hotSettings"
        height="calc(100vh - 150px)"
      >
        <hot-column
          title="Email address"
          validator="email-address"
        ></hot-column>
        <hot-column title="Full name"></hot-column>
      </hot-table>
    </b-container>

    <b-modal
      id="import-file-modal"
      centered
      title="Import file"
      size="lg"
      ok-title="Import"
      ok-only
      :ok-disabled="!importFileShowPreview"
      v-on:hide="onModalHide"
    >
      <b-container fluid>
        <b-form-row class="align-items-center mb-2">
          <b-col>
            <b-form-group>
              <b-form-file
                placeholder="Choose a file or drop it here (csv, xls, xlsx, ...)"
                drop-placeholder="Drop file here..."
                accept="text/csv,application/vnd.ms-excel,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                v-on:input="onFileChange"
              ></b-form-file>
            </b-form-group>
          </b-col>
          <b-col cols="auto">
            <b-form-group>
              <b-form-checkbox id="has-headers" v-model="importFileHasHeaders">
                File has a header row
              </b-form-checkbox>
            </b-form-group>
          </b-col>
        </b-form-row>
        <b-form-row v-if="importFileRows" class="mb-2">
          <b-col>
            <b-form-group label="Email address">
              <b-form-select
                v-model="importFileEmailAddressIdx"
                :options="importFileHeaders"
              >
              </b-form-select>
            </b-form-group>
          </b-col>
          <b-col>
            <b-form-group label="Full name">
              <b-form-select
                v-model="importFileFullNameIdx"
                :options="importFileHeaders"
              >
              </b-form-select>
            </b-form-group>
          </b-col>
        </b-form-row>
        <div v-show="importFileShowPreview">
          <span>
            <font-awesome-icon icon="search" class="preview-icon" />
            Preview</span
          >
          <hot-table
            :data="importFilePreviewData"
            :settings="importFileHotSettings"
          >
            <hot-column title="Email address"></hot-column>
            <hot-column title="Full name"></hot-column>
          </hot-table>
        </div>
      </b-container>
    </b-modal>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Watch } from "vue-property-decorator";
import ApiService from "@/services/api";
import { HotTable, HotColumn } from "@handsontable/vue";
import XLSX from "xlsx";
import _ from "lodash";
import { BvModalEvent } from "bootstrap-vue";
import Handsontable from "handsontable";

import { library } from "@fortawesome/fontawesome-svg-core";
import {
  faFileImport,
  faPaperPlane,
  faSearch
} from "@fortawesome/free-solid-svg-icons";
library.add(faFileImport, faPaperPlane, faSearch);

const PREVIEW_SIZE = 3;

const EMAIL_ADDRESS_RE = /^(([^<>()[\].,;:\s@"]+(\.[^<>()[\].,;:\s@"]+)*)|(".+"))@(([^<>()[\].,;:\s@"]+\.)+[^<>()[\].,;:\s@"]{2,})$/i;

Handsontable.validators.registerValidator(
  "email-address",
  (query, callback) => {
    callback(query !== null && EMAIL_ADDRESS_RE.test(query));
  }
);

@Component({ components: { HotTable, HotColumn } })
export default class SendInvitations extends Vue {
  hotSettings = {
    width: "100%",
    licenseKey: "non-commercial-and-evaluation",
    stretchH: "all",
    rowHeaders: true,
    minSpareRows: 1,
    // Do not validate the spare row at the end
    afterValidate: (isValid: boolean, value: any, row: number) => {
      const n = this.hotInstance?.countRows() || 0;
      if (row == n - 1) return true;
      return isValid;
    },
    contextMenu: {
      items: {
        ["row_below"]: {},
        ["row_above"]: {},
        ["remove_row"]: {}
      }
    }
  };
  hasErrors = false;
  rows: [string, string][] = [];

  onSend() {
    this.hotInstance?.validateCells(isValid => {
      this.hasErrors = !isValid;
      if (isValid) {
        const invitations = this.rows.map(r => ({
          emailAddress: r[0],
          fullName: r[1]
        }));
        // Sanity check before removing the spare row at the end
        const n = invitations.length;
        if (
          _.isEmpty(invitations[n - 1].emailAddress) &&
          _.isEmpty(invitations[n - 1].fullName)
        ) {
          invitations.pop();
        }
        if (!_.isEmpty(invitations)) {
          ApiService.sendInvitations(invitations).then(() =>
            this.$router.push({ name: "Invitations" })
          );
        }
      }
    });
  }

  get hotInstance(): Handsontable | null {
    return (this.$refs.table as any)?.hotInstance;
  }

  // Import File

  importFileRows: string[][] | null = null;
  importFileHasHeaders = true;
  importFileEmailAddressIdx = -1;
  importFileFullNameIdx = -1;
  importFileHotSettings = {
    licenseKey: "non-commercial-and-evaluation",
    rowHeaders: true,
    stretchH: "all",
    width: "100%",
    height: (PREVIEW_SIZE + 1) * 25 + 20,
    readOnly: true
  };

  @Watch("importFileHeaders")
  onHeadersChange() {
    this.importFileEmailAddressIdx = -1;
    this.importFileFullNameIdx = -1;
  }

  get importFileHeaders(): { value: number; text: string }[] {
    const sheet = this.importFileRows;
    if (sheet === null || sheet.length == 0) {
      return [];
    }
    let names;
    if (this.importFileHasHeaders) {
      names = sheet[0];
    } else {
      names = _.range(sheet[0].length).map(XLSX.utils.encode_col);
    }
    const headers = names.map((h, i) => ({ value: i, text: h }));
    headers.splice(0, 0, { value: -1, text: "--Not Assigned--" });
    return headers;
  }

  emailAddressHeaderInRange() {
    const ncols: number = _.get(this.importFileRows, "0.length") || 0;
    return _.inRange(this.importFileEmailAddressIdx, 0, ncols);
  }

  fullNameHeaderInRange() {
    const ncols: number = _.get(this.importFileRows, "0.length") || 0;
    return _.inRange(this.importFileFullNameIdx, 0, ncols);
  }

  headersInRange() {
    return this.emailAddressHeaderInRange && this.fullNameHeaderInRange;
  }

  get importFileShowPreview(): boolean {
    return (
      this.importFileRows !== null &&
      (this.emailAddressHeaderInRange() || this.fullNameHeaderInRange())
    );
  }

  get importFilePreviewData(): string[][] {
    const rows = this.importFileRows;
    if (rows === null) {
      return [];
    }
    return this.extractDataFromImportedFile(PREVIEW_SIZE);
  }

  onModalHide(ev: BvModalEvent) {
    if (ev.trigger === "ok") {
      this.rows = this.extractDataFromImportedFile();
    }
    this.importFileRows = null;
  }

  extractDataFromImportedFile(nrows?: number): [string, string][] {
    let rows = this.importFileRows;
    if (rows === null) {
      return [];
    }
    if (this.importFileHasHeaders) {
      rows = rows.slice(1, nrows && nrows + 1);
    } else {
      rows = rows.slice(0, nrows);
    }
    return rows.map(r => [
      r[this.importFileEmailAddressIdx] || "",
      r[this.importFileFullNameIdx] || ""
    ]);
  }

  onFileChange(file: File) {
    const reader = new FileReader();
    reader.onload = (e: ProgressEvent<FileReader>) => {
      if (e?.target === null) {
        return;
      }
      const bstr = e.target.result;
      const wb = XLSX.read(bstr, { type: "binary" });
      const wsname = wb.SheetNames[0];
      const ws = wb.Sheets[wsname];
      this.importFileRows = XLSX.utils.sheet_to_json(ws, {
        header: 1
      });
    };
    reader.readAsBinaryString(file);
  }
}
</script>

<style lang="scss">
.handsontable td.htInvalid {
  background-color: var(--danger) !important;
}

.send-invitations .invalid-data {
  font-size: 80%;
  color: var(--danger);
}

.preview-icon {
  font-size: 12px;
  vertical-align: middle;
}

.send-invitations {
  .handsontable td,
  .handsontable th {
    line-height: 28px;
  }
}
</style>
