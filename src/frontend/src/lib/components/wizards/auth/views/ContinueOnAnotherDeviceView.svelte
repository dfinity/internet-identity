<script lang="ts">
  import { RegisterAccessMethodWizard } from "$lib/components/wizards/registerAccessMethod";

  interface Props {
    registrationId?: string;
    onRegistered: (identityNumber: bigint) => void;
    onError: (error: unknown) => void;
  }

  let { registrationId, onRegistered, onError }: Props = $props();
</script>

<RegisterAccessMethodWizard {registrationId} {onRegistered} {onError} />
