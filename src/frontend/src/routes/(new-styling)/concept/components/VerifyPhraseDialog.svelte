<script lang="ts" module>
  /** The phrase this concept's identities wrote down: one word per letter of
   *  the alphabet, in order, so whoever tries the concept can type it. */
  const CONCEPT_PHRASE = [
    "acid",
    "bamboo",
    "canvas",
    "desert",
    "eagle",
    "fabric",
    "galaxy",
    "harbor",
    "island",
    "jungle",
    "kitten",
    "lemon",
    "marble",
    "noble",
    "orbit",
    "pepper",
    "quantum",
    "rocket",
    "salmon",
    "timber",
    "umbrella",
    "velvet",
    "walnut",
    "zebra",
  ];
</script>

<script lang="ts">
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { CreateRecoveryPhraseWizard } from "$lib/components/wizards/createRecoveryPhrase";

  interface Props {
    onVerified: () => void;
    onClose: () => void;
  }

  const { onVerified, onClose }: Props = $props();

  // The product's wizard, as /manage/recovery opens it for a phrase set up
  // earlier: Internet Identity doesn't keep the words, so the user types them
  // from their copy. The product checks them with a call signed by the phrase
  // and waits two seconds at least, so "Checking" doesn't flash.
  const verify = async (entered: string[]): Promise<boolean> => {
    await new Promise((resolve) => setTimeout(resolve, 2000));
    const correct = entered.join(" ") === CONCEPT_PHRASE.join(" ");
    if (correct) {
      onVerified();
    }
    return correct;
  };
</script>

<Dialog {onClose} closeOnOutsideClick={false}>
  <CreateRecoveryPhraseWizard
    action="verify"
    onCreate={() => Promise.resolve()}
    onVerify={verify}
    onUnlock={() => Promise.resolve(false)}
    onCancel={onClose}
    onError={onClose}
  />
</Dialog>
