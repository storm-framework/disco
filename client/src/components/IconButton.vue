<template functional>
  <b-button class="icon-button" v-bind="data.attrs" v-on="listeners">
    <font-awesome-icon class="icon" :icon="props.icon" />
    <slot />
  </b-button>
</template>

<script lang="ts">
export default {
  functional: true,
  props: {
    icon: {
      required: true
    }
  }
};
</script>

<style lang="scss" scoped>
.icon-button {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  line-height: 100%;
  height: 1.5em;
  box-sizing: content-box;
}

.icon {
  margin-right: 0.5em;
}
</style>
