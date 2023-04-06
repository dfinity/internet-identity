/** A showcase of common CSS patterns that can be reuses all all over the app */
import "./styles/main.css";
import { html } from "lit-html";
import {
  icLogo,
  settingsIcon,
  dropdownIcon,
  copyIcon,
  warningIcon,
  checkmarkIcon,
} from "./components/icons";
import { warnBox } from "./components/warnBox";
import { irregularity } from "./components/irregularity";
import { toast } from "./components/toast";
import { modal } from "./components/modal";
import { dappsTeaser, DappDescription } from "./components/dappList";

// these words are never longer than 8 characters
// according to the bip39 spec
// https://getcoinplate.com/blog/official-bip39-word-list-mnemonic-in-english-verified/
// we do include the anchor number as a first word
const recoveryWords = [
  "1988236",
  "gloom",
  "squirrel",
  "candy",
  "police",
  "nicolas",
  "rebel",
  "ocelot",
  "vampire",
  "pasta",
  "sister",
  "castle",
  "cinnamon",
  "glue",
  "potato",
  "own",
  "problem",
  "evolve",
  "door",
  "country",
  "basket",
  "lyrics",
  "tuna",
  "catch",
  "tongue",
];

const getDappsMeta = async (): Promise<DappDescription[]> => {
  const dappsList = await fetch("/dapps.json").then((d) => d.json());
  return dappsList;
};

/**
 * Fetch the actual information at some point?
 **/

const dappsMeta: DappDescription[] = [
  {
    id: "dscvr",
    name: "DSCVR",
    oneLiner: "Web3 alternative to Reddit with user governed Portals",
    link: "https://dscvr.one/",
    derivationOrigin: "https://h5aet-waaaa-aaaab-qaamq-cai.raw.ic0.app",
    description:
      "DSCVR is an end-to-end decentralized Web3 social media platform that allows communities to form into groups called Portals. These Portals can be NFT gated, airdrop fungible and non-fungible tokens to their members and much more. DSCVR also allows for tipping posts in a growing number of cryptos, supporting ckBTC, a Bitcoin twin living on the Internet Computer.",
    logo: "dscvr_logo.webp",
  },
  {
    id: "distrikt",
    name: "Distrikt",
    oneLiner: "100% on-chain microblogging social media platform",
    link: "https://az5sd-cqaaa-aaaae-aaarq-cai.ic0.app",
    description:
      "Distrikt is a completely decentralized, community-owned Web3 social media platform. Users of the platform will soon be able vote on upgrades, and no user data will ever be mined or sold. Create your account, secured by Internet Identity today.",
    logo: "distrikt_logo.webp",
  },
  {
    id: "kontribute",
    name: "Kontribute",
    oneLiner: "Web3 storytelling",
    link: "https://kontribute.app",
    derivationOrigin: "https://3ezq7-iqaaa-aaaal-aaacq-cai.raw.ic0.app",
    description:
      "Kontribute is a web3 creators platform that brings story writing and digital art collectibles together. Features include: decentralized story storage, likes, tipping, polls, NFT marketplace and NFT minting.",
    logo: "kontribute_logo.webp",
  },
  {
    id: "kinic",
    name: "Kinic",
    oneLiner: "The world's first web3 search engine",
    link: "https://74iy7-xqaaa-aaaaf-qagra-cai.ic0.app",
    description: "The world’s first Web3 search engine",
    logo: "kinic_logo.webp",
  },
  {
    id: "openchat",
    name: "OpenChat",
    oneLiner: "Decentralized alternative to WhatsApp",
    link: "https://oc.app/",
    derivationOrigin: "https://6hsbt-vqaaa-aaaaf-aaafq-cai.ic0.app",
    description:
      "What if you could own a piece of WhatsApp and vote on what features get added? OpenChat is a fully decentralized real-time messaging service that is indistinguishable from Web2 chat apps, while living 100% on the blockchain. This allows users to send crypto to each other - including Bitcoin - and soon to own a part of OpenChat through CHAT tokens.",
    logo: "openchat_logo.webp",
  },
  {
    id: "uniswapfrontendontheic",
    name: "Uniswap Front End on the IC",
    link: "https://uniswap-on-ic.xyz",
    description:
      "Uniswap's frontend hosted on the Internet Computer with canister-based wallet integration. Thanks to the Internet Computer, traditional DeFi solutions can now be completely decentralized, having their frontend hosted on ICP. There is no longer a need to include centralized cloud providers in a decentralized application.",
    oneLiner: "Front-End On-Chain",
    logo: "uniswapfrontendontheic_logo.webp",
  },
  {
    id: "seers",
    name: "Seers",
    link: "https://seers.social/",
    derivationOrigin: "https://oulla-fyaaa-aaaag-qa6fa-cai.ic0.app",
    description:
      "What if there was decentralized Twitter that included prediction markets? Seers is Web3 social media platform hosted 100% on-chain combining social media features with prediction markets.",
    logo: "seers_logo.webp",
  },
  {
    id: "papyrs",
    name: "Papyrs",
    link: "https://app.papy.rs",
    description:
      "An open-source, privacy-first, decentralized blogging platform that lives 100% on chain. Have an unstoppable voice and full control over your data.",
    logo: "papyrs_logo.webp",
  },
  {
    id: "taggr",
    name: "Taggr",
    link: "https://6qfxa-ryaaa-aaaai-qbhsq-cai.ic0.app",
    description:
      "Fully on-chain and fully autonomous SocialFi network. A simple way to publish content on a public compute infrastructure. No Ponzinomics - TAGGR has a sustainable tokenomics model that rewards quality posts and removes incentive to spam.",
    logo: "taggr_logo.webp",
  },
  {
    id: "funded",
    name: "Funded",
    link: "https://funded.app",
    description:
      "Web3 crowdfunding! Thanks to ICP's low transaction fees and advanced smart contract technology, you can participate in crowdfunding with ICP, BTC and ETH without worrying about losing money on gas fees.",
    logo: "funded_logo.webp",
  },
  {
    id: "contentfly",
    name: "Content Fly",
    link: "https://contentfly.app/",
    derivationOrigin: "https://main.contentfly.app",
    description:
      "Content Fly is a Web3 Job Management Tool & Marketplace. It allows content buyers & creators to work together with the security of an escrow payment and DAO based dispute resolution. IP is protected and transferred as an NFT.",
    logo: "contentfly_logo.webp",
  },
  {
    id: "icdex",
    name: "ICDex",
    link: "https://avjzx-pyaaa-aaaaj-aadmq-cai.raw.ic0.app/ICDex",
    derivationOrigin: "https://avjzx-pyaaa-aaaaj-aadmq-cai.raw.ic0.app",
    description:
      "ICDex is flagship product by ICLighthouse, an orderbook based DEX that runs 100% on-chain. The world's first orderbook DEX - made possible by advanced ICP smart contracts",
    logo: "icdex_logo.webp",
  },
  {
    id: "unfoldvr",
    name: "UnfoldVR",
    oneLiner: "Decentralizing asset Creation and Discovery for the Metaverse",
    link: "https://jmorc-qiaaa-aaaam-aaeda-cai.ic0.app",
    description:
      "UnfoldVR empowers creators to author 3D NFTs using easy-to-use tools both on the Web and in Virtual Reality.",
    logo: "unfoldvr_logo.webp",
  },
  {
    id: "yumi",
    name: "Yumi",
    oneLiner:
      "Yumi is an ultra-fast, low-cost, and 100% decentralized NFT marketplace on ICP.",
    link: "https://tppkg-ziaaa-aaaal-qatrq-cai.raw.ic0.app",
    description:
      "Yumi is a high-speed, low-cost, and fully decentralized NFT marketplace built on the Internet Computer. All digital collectibles available on Yumi are hosted fully on-chain. The minting of NFTs is completely free for creators (no gas fees).",
    logo: "yumi_logo.webp",
  },
  {
    id: "canlista",
    name: "Canlista",
    oneLiner: "Internet Computer Canister Registry",
    link: "https://k7gat-daaaa-aaaae-qaahq-cai.ic0.app",
    description:
      "The Internet Computer community canister registry. Find, publish and extend applications and services built on the Internet Computer. Log in with Internet Identity. ",
    logo: "canlista_logo.webp",
  },
  {
    id: "missionispossible",
    name: "Mission Is Possible",
    link: "https://to3ja-iyaaa-aaaai-qapsq-cai.raw.ic0.app",
    description:
      "Mission is Possible - 3rd place winner of the DSCVR Hackathon Season 2 - is a PVP third person shooter hosted on the Internet Computer blockchain. The John Wick inspired game is built using the Unity 3D Game Engine, and hosted on the IC enabling decentralized login with Internet Identity. ",
    oneLiner: "3rd Place DSCVR Hackathon",
    logo: "missionispossible_logo.webp",
  },
  {
    id: "catalyze",
    name: "Catalyze",
    link: "https://aqs24-xaaaa-aaaal-qbbea-cai.ic0.app",
    description:
      "Catalyze is a decentralized social and community-building platform designed to host engaged and thriving Web3 communities. With a unique and customized engagement economy, Catalyze communities and their members will be rewarded for their participation and contribution. Main features include: direct communication, event & task management, integrated Web3 wallets, NFT Gating, NFT airdrop & sales management.",
    oneLiner: "Web3 social media platform for communities",
    logo: "catalyze_logo.webp",
  },
  {
    id: "icme",
    name: "ICME",
    link: "https://sygsn-caaaa-aaaaf-qaahq-cai.raw.ic0.app",
    description:
      "ICME is a no-code tool that makes it easy for anyone to build and deploy beautiful websites on the Internet Computer. Launch your blog or business's website on the Internet Computer today.",
    logo: "icme_logo.webp",
  },
  {
    id: "sagatarot",
    name: "Saga Tarot",
    link: "https://5nl7c-zqaaa-aaaah-qaa7a-cai.raw.ic0.app/",
    derivationOrigin: "https://l2jyf-nqaaa-aaaah-qadha-cai.raw.ic0.app",
    description:
      "Have your fortune told on the Internet Computer. Saga Tarot gives you a tarot reading in one click. The user-friendly dapp is built completely on the Internet Computer, accessible from any browser. What will the future hold for you?",
    logo: "sagatarot_logo.webp",
  },
  {
    id: "icdrive",
    name: "IC Drive",
    link: "https://rglue-kyaaa-aaaah-qakca-cai.ic0.app",
    description:
      "A decentralized private file storage dapp built on the Internet Computer. Store and securely share any type from anywhere in the world with this decentralized version of Box, or Google Drive. ",
    logo: "icdrive_logo.webp",
  },
  {
    id: "crowdgovorg",
    name: "CrowdGov.org",
    oneLiner: "The simplified, one stop shop for IC Governance.",
    link: "https://crowdgov.org",
    description:
      "The crowdgov.org website is dedicated to simplified governance for the internet computer. You will find information about how to participate in governance and how to maximize voting rewards. A variety of research tools are provided to help you learn more about NNS ecosystem participants and the current state of decentralization.",
    logo: "crowdgovorg_logo.webp",
  },
  {
    id: "nnsfront-enddapp",
    name: "NNS Front-End Dapp",
    oneLiner: "Dapp for Staking Neurons + Voting On-Chain",
    link: "https://nns.ic0.app",
    description:
      "The NNS front-end dapp allows anyone to interact with the Internet Computer's Network Nervous System with a user-friendly UI. Served completely end-to-end through blockchain, this dapp allows you to manage ICP, stake neurons, participate in voting, and earn governance rewards.",
    logo: "nnsfront-enddapp_logo.webp",
  },
  {
    id: "tipjar",
    name: "Tipjar",
    link: "https://tipjar.rocks",
    derivationOrigin: "https://k25co-pqaaa-aaaab-aaakq-cai.ic0.app",
    description:
      "A tool to donate cycles to canisters as well as keep them monitored.",
    logo: "tipjar_logo.webp",
  },
  {
    id: "aedile",
    name: "Aedile",
    link: "https://eemeo-taaaa-aaaad-qakjq-cai.ic.fleek.co/",
    derivationOrigin: "https://vqdn4-miaaa-aaaaf-qaawa-cai.ic0.app",
    description:
      "Aedile brings your team's work together in one shared space, completely built on-chain. Manage your boards, columns & cards to transform your projects, serving web experiences directly from the Internet Computer blockchain. ",
    logo: "aedile_logo.webp",
  },
  {
    id: "riseofthemagni",
    name: "Rise of the Magni",
    link: "https://riseofthemagni.com/",
    description:
      "Rise of the Magni, built by Toniq Labs, winner of the DSCVR hackathon for games on the Internet Computer. Buy, earn, and trade collectibles, compete in tactical battles online to earn in-game tokens, and venture through story mode to experience one of the first games built on the Internet Computer.",
    logo: "riseofthemagni_logo.webp",
  },
  {
    id: "nuance",
    name: "Nuance",
    link: "https://exwqn-uaaaa-aaaaf-qaeaa-cai.ic0.app/",
    description:
      "Nuance is a Web3.0 blogging platform that is hosted on-chain end-to-end on the Internet Computer. Developed by Aikin Dapps, the alpha of the world's first blogging platform to be hosted entirely on a blockchain has now launched. Nuance aims to bring NFTs into the world of editorial content ownership.",
    logo: "nuance_logo.webp",
  },
  {
    id: "modclub",
    name: "MODCLUB",
    link: "https://ljyte-qiaaa-aaaah-qaiva-cai.raw.ic0.app",
    description:
      "MODCLUB is a decentralized moderation tool based hosted fully on-chain. Built on the Internet Computer, MODCLUB rewards users for effectively moderating content. Currently in beta stages of their solution, users will be rewarded in tokens for moderating their favorite communities.",
    logo: "modclub_logo.webp",
  },
  {
    id: "nftanvil",
    name: "NFTAnvil",
    link: "https://nftanvil.com",
    description:
      "NFTAnvil is a wallet, mint & marketplace in the Anvil ecosystem. It's built from scratch and has an alternative & genuine approach to NFTs. It uses Anvil's auto-scaling multi-canister token architecture.",
    logo: "nftanvil_logo.webp",
  },
  {
    id: "icpswap",
    name: "ICPSwap",
    link: "https://icpswap.com",
    derivationOrigin: "https://app.icpswap.com",
    description:
      "ICPSwap is DEX built completely end-to-end on-chain. By building the ability for anyone to swap tokens through ICPSwap leveraging the Internet Computer blockchain as the high-speed, scalable, low-cost infrastructure makes ICPSwap a first-to-market in the growing Internet Computer DeFi ecosystem.",
    logo: "icpswap_logo.webp",
  },
  {
    id: "dmail",
    name: "Dmail",
    oneLiner: "Web3 Decentralized Email Client",
    link: "https://dmail.ai/",
    derivationOrigin: "https://evyc3-ziaaa-aaaak-aam5a-cai.ic0.app",
    description:
      "Dmail is the Web3 replacement for e-mail. Hosted completely on-chain and built on the Internet Computer, this dapp enables users to send and receive blockchain-backed, encrypted messages. In addition, Dmail addresses are owned by users as NFT assets - there is a natively built marketplace. Dmail was the winner of the 2021 Warpspeed ICP Hackathon in China, and saw an immediate round of funding netting a $10M valuation. ",
    logo: "dmail_logo.webp",
  },
  {
    id: "dsocial",
    name: "DSocial",
    link: "https://DSocial.app",
    derivationOrigin: "https://dwqte-viaaa-aaaai-qaufq-cai.ic0.app",
    description:
      "DSocial is a decentralized version of YouTube -- enabling content creators to be fairly rewarded for their work, and engagement. This Web3 media platform is hosted end-to-end on the Internet Computer interoperating with Arweave for decentralized video content.",
    logo: "dsocial_logo.webp",
  },
  {
    id: "icnaming",
    name: "ICNaming",
    link: "https://app-testnet.icnaming.com/",
    description:
      "ICNaming is a testnet that is enabling the Internet Computer ecosystem to register domain names on the Internet Computer Name Service. Similar to the Ethereum Name Servce (ENS), ICNaming aims to offer a decentralized name service for users to pseudonomize their wallet addresses on ICP, as well as domain names, and canister smart contract IDs. ",
    logo: "icnaming_logo.webp",
  },
  {
    id: "stoicwallet",
    name: "Stoic Wallet",
    link: "https://www.stoicwallet.com/",
    description:
      "Stoic Wallet by Toniq Labs allows anyone to create a digital wallet, authenticating users through a variety of methods, one of those being Internet Identity. Create accounts, keep an address book, and more. ",
    logo: "stoicwallet_logo.webp",
  },
  {
    id: "departurelabs",
    name: "Departure Labs",
    link: "https://uhmvd-qqaaa-aaaam-aavna-cai.ic0.app",
    description:
      "Departure Labs is exploring on-chain media, web native NFTs, and developing productized open internet services for developers and consumers. Departure Labs is currently developing a non-fungible token standard that leverages the unique properties of the Internet Computer and enables builders to create entire experiences from a single contract.\n",
    logo: "departurelabs_logo.webp",
  },
  {
    id: "factland",
    name: "Factland DAO",
    oneLiner:
      "A Web3 community building decentralized trust in the age of misinformation",
    link: "https://factland.org",
    derivationOrigin: "https://demo.factland.org",
    description:
      "Factland is a Web3 DAO with a mission to slow the spread of misinformation online. Factland makes it easy for anyone to flag untrustworthy claims and have them promptly adjudicated by a decentralized community of fact checkers rewarded in crypto.",
    logo: "factland_logo.png",
  },
  {
    id: "hot-or-not",
    name: "Hot or Not",
    oneLiner: "Web3's answer to TikTok with speculation on short video content",
    link: "https://hotornot.wtf",
    description:
      "Hot or Not is a decentralized short-video social media (like TikTok) which integrates prediction markets for content. In addition to creating and sharing short videos on the platform, the users can also speculate on the short videos uploaded by other users on the platform. The users can vote whether a video would be 'Hot' or 'Not' and stake blockchain tokens to substantiate their vote. The results are declared every hour and the winners are rewarded with 2x tokens. With Decentralized governance and content moderation, Hot or Not would put the users at the center of the ecosystem",
    logo: "hot_or_not_logo.webp",
  },
];

export const styleguide = html`
  <style>
    .styleguide {
      margin: 10rem auto;
      max-width: 60rem;
      padding: 0 2rem;
    }
    .styleguide code {
      background-color: #202124;
      color: #fff;
      padding: 0.2em;
    }

    .demo {
      z-index: 0;
      position: relative;

      min-height: 15rem;

      margin: 2rem 0;
      border: rgba(255, 255, 255, 0) 4px solid;
      padding: 1em;
      background-color: #f7f7f7;
      background-image: linear-gradient(
          45deg,
          rgba(0, 0, 0, 0.05) 25%,
          transparent 25%,
          transparent 75%,
          rgba(0, 0, 0, 0.05) 75%,
          rgba(0, 0, 0, 0.05)
        ),
        linear-gradient(
          45deg,
          rgba(0, 0, 0, 0.05) 25%,
          transparent 25%,
          transparent 75%,
          rgba(0, 0, 0, 0.05) 75%,
          rgba(0, 0, 0, 0.05)
        );
      background-size: 3rem 3rem;
      background-position: 0 0, 1.5rem 1.5rem;
      border-radius: 0.5em;

      box-shadow: inset 0 0 1rem rgba(0, 0, 0, 0.5);
    }

    .demo-section {
      background: #fff;
      padding: 2.5rem;
      border-radius: 0.5em;
      box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.1);
    }
  </style>
  <section class="styleguide">
    <h1 class="t-title t-title--main">Design Patterns</h1>
    <article class="l-stack c-card c-card--highlight">
      <h1 class="t-title t-title--main">Typography</h1>
      <p class="t-lead">
        The font used all over the app is called "Montserrat" and is loaded from
        <a href="https://fonts.google.com/specimen/Montserrat">Google Fonts</a>.
        It is a sans-serif font that is easy to read.
      </p>
      <p class="t-paragrapgh">
        The base font size is dependent on the browser's window size. But 1rem
        is our base unit. To make it easier to work with it, we consider 1rem
        beeing the equivalent of ~10px.
      </p>
      <p class="t-paragrapgh">
        Since we use a CSS reset almost no html elements come with a default
        styling. To help style text element we have a bunch of utility classes
        that can be used.
      </p>
      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Tiltes</h2>
        <section class="demo" aria-label="Titles Demo">
          <h1 class="t-title t-title--main">Large Title</h1>
          <h2 class="t-title">Default regular title</h2>
        </section>
        <p class="t-lead">
          The <code>.t-title</code> class can be used to style titles. If you
          want to make it a main title, add the
          <code>.t-title--main</code> modifier class.
        </p>
      </aside>
      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Paragraphs</h2>
        <section class="demo" aria-label="Paragraphs Demo">
          <p class="t-lead">This is a lead paragraph</p>
          <p class="t-paragraph">This is a regular paragraph</p>
        </section>
        <p class="t-lead">
          There are two types of text paragraphs. The <code>.t-lead</code> class
          can be used to style lead paragraphs that follow a title. The
          <code>.t-paragraph</code> class is a more generic class that can be
          used to style any other paragraph.
        </p>
      </aside>
      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Various text helpers</h2>
        <section class="demo" aria-label="Verious elements Demo">
          <p class="t-paragraph">
            <a href="#">This is an actual "a" tag</a><br />
            <button class="t-link">
              This is a button element that looks like a link</button
            ><br />
            <button class="t-link t-link--discreet">
              This is a link that is not underlined</button
            ><br />
            <a href="#" class="t-link"
              ><i class="t-link__icon">+</i>Link with icon</a
            ><br />
          </p>
        </section>
        <p class="t-lead">
          There are a bunch of other text helpers that can be used to style text
          within a paragraph or anywhere else. One of the rare elements that has
          a default style is a <a href="#">link</a>, but sometimes you might
          want an other element to look like a link. In that case you can use
          the <code>.t-link</code> class. In some rare cases we want links to
          look less prominent (by removing the underline) you can use the
          <code>.t-link--discreet</code> class.
        </p>
      </aside>
    </article>

    <article class="l-stack  c-card c-card--highlight">
      <h1 class="t-title t-title--main">Layout</h1>
      <p class="t-lead">
        We use a bunch of layout classes to style the
        <code>.c-card__icon</code> of the app. They just define where elements
        are placed.
      </p>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Sections</h2>
        <p class="t-lead">
          A section, much like a text-paragraph, is a visually distinct area of
          the app. In our case it just sets a space to the top. It can be used
          with the <code>.l-stack</code> class. Sometimes we want there to be
          more spaces between sections. In that case we can use the
          <code>.l-stack--spacious</code> modifier class.
        </p>
      </aside>
    </article>

    <article class="l-stack c-card c-card--highlight">
      <h1 class="t-title t-title--main">Components</h1>
      <p class="t-lead">
        Components are the building blocks of the app. They consist of visual
        patterns that are used all over the app.
      </p>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Cards</h2>
        <section class="demo" aria-label="Cards Demo">
          <div class="c-card l-stack">
            <h2 class="t-title">Default card</h2>
          </div>
          <div class="c-card c-card--narrow l-stack">
            <h2 class="t-title">Narrow Card</h2>
          </div>
          ${warnBox({
            title: "Warning Card with Icon",
            message: "This is a card with a warning icon",
            additionalClasses: ["l-stack"],
          })}
          ${warnBox({
            title: "Actionable warning",
            message:
              "Sometimes we want to give the user the option to do something about the warning",
            additionalClasses: ["l-stack"],
            slot: html`<div class="c-button-group">
              <button class="c-button c-button--primary">Do something!</button>
              <button class="c-button c-button--secondary">Ignore</button>
            </div>`,
          })}
        </section>
        <p class="t-lead">
          The card component is used to group content. It can be used with the
          <code>.c-card</code> class. It comes with a few modifiers that can be
          used to style it differently.
        </p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Dapp List</h2>
        <section class="demo" aria-label="Dapp List Demo">
          ${dappsTeaser(dappsMeta)}
        </section>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title">Form Elements</h2>
        <section class="demo" aria-label="Form Elements Demo">
          <label class="c-input--anchor__wrap" aria-label="Identity Anchor">
            <input
              type="text"
              data-role="anchor-input"
              class="c-input c-input--vip c-input--spacious c-input--anchor"
              placeholder="Enter anchor"
            />
          </label>

          <input type="text" placeholder="Text Input" class="c-input" />
          <div class="c-input">DIV as c-input</div>
          <div class="c-input c-input--readonly">Readonly input</div>
          <div class="c-input c-input--vip">Important inputs</div>
          <input type="text" placeholder="Rounded Input" class="c-input" />
          <input
            type="text"
            placeholder="Spacious Input"
            class="c-input c-input--spacious"
          />
          <input
            type="text"
            placeholder="Centered Input"
            class="c-input c-input--centered"
          />
          <input
            type="text"
            placeholder="Errored Input"
            class="c-input has-error"
          />

          <button class="c-button">Primary Button</button>
          <button class="c-button c-button--secondary">Secondary Button</button>
          <button class="c-button c-button--disabled" disabled>
            Disabled Button
          </button>
          <button class="c-button c-button--warning">Warning Button</button>
        </section>
        <p class="t-lead">
          Form elements are used to collect information from the user. But we
          also use the classes to show important numbers and other information.
        </p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title">Lists</h2>
        <section class="demo" aria-label="List Elements Demo">
          <ul class="c-list">
            <li>Default list item I</li>
            <li>Default list item II</li>
            <li>Default list item III</li>
          </ul>

          <ul class="c-list c-list--bulleted l-stack">
            <li>Bulleted list item I</li>
            <li>Bulleted list item II</li>
            <li>Bulleted list item III</li>
          </ul>

          <ul class="c-list c-list--numbered l-stack">
            <li>Numbered list item I</li>
            <li>Numbered list item II</li>
            <li>Numbered list item III</li>
          </ul>
        </section>
        <p class="t-lead">
          The <code>.c-list</code> class is used to style list elements. It
          expects a <code>&lt;li&gt;</code> element as a direct child.
        </p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Recovery Word List</h2>
        <section class="demo" aria-label="Recovery List Elements Demo">
          <output class="c-input c-input--recovery">
            <ol class="c-list c-list--recovery">
              ${recoveryWords.map((word, i) => {
                // loop through the demo recovery words and add some classes to
                // demonstrate the different states
                const classes = ["c-list--recovery-word"];
                let isEditable = false;
                let icon: undefined | "warning" | "check";
                let text = word;

                if (i === 21) {
                  classes.push("c-list--recovery-word__attention");
                  isEditable = true;
                  text = "";
                }

                if (i === 22) {
                  classes.push("c-list--recovery-word__incorrect");
                  isEditable = true;
                  icon = "warning";
                }

                if (i === 23) {
                  classes.push("c-list--recovery-word__correct");
                  isEditable = true;
                  icon = "check";
                }

                if (i === 24) {
                  classes.push("c-list--recovery-word__disabled");
                }

                return html`<li
                  class=${classes.join(" ")}
                  style="--index: '${i + 1}';"
                >
                  ${icon != null
                    ? html`<i class="c-list--recovery-word__icon"
                        >${icon === "warning" ? warningIcon : checkmarkIcon}</i
                      >`
                    : null}
                  ${isEditable === true
                    ? html`<input
                          type="text"
                          class="c-recoveryInput"
                          value=${text}
                          maxlength="8"
                        />&nbsp;`
                    : // &nbsp; is used to prevent the element from collapsing
                      // when the input is empty, especially in Safari
                      text}
                </li>`;
              })}
            </ol>
            <i
              aria-label="Copy phrase to clipboard"
              title="Copy phrase to clipboard"
              tabindex="0"
              class="c-button__icon"
            >
              <span>Copy</span>
              ${copyIcon}
            </i>
          </output>
        </section>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Actionable Lists</h2>
        <section class="demo" aria-label="Details / Summary Demo">
          <ul class="c-action-list">
            <li class="c-action-list__item">
              <span class="c-action-list__label">Actionable List Item</span>
              <span class="c-action-list__action c-dropdown">
                <button
                  class="c-dropdown__trigger"
                  aria-expanded="false"
                  aria-controls="dropdown-i"
                >
                  ${settingsIcon}
                </button>
                <ul class="c-dropdown__menu" id="dropdown-i">
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Delete Anchor</button>
                  </li>
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Lock Anchor</button>
                  </li>
                </ul>
              </span>
            </li>
          </ul>

          <ul class="c-action-list">
            <li class="c-action-list__item">
              <span class="c-action-list__label">Actionable List Item</span>
              <span class="c-action-list__action c-dropdown">
                <button
                  class="c-dropdown__trigger"
                  aria-expanded="false"
                  aria-controls="dropdown-i"
                >
                  ${dropdownIcon}
                </button>
                <ul class="c-dropdown__menu" id="dropdown-i">
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Delete Anchor</button>
                  </li>
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Lock Anchor</button>
                  </li>
                </ul>
              </span>
            </li>
            <li class="c-action-list__item">
              <span class="c-action-list__label">Actionable List Item</span>
              <span class="c-action-list__action c-dropdown">
                <button
                  class="c-dropdown__trigger"
                  aria-expanded="false"
                  aria-controls="dropdown-i"
                >
                  ${dropdownIcon}
                </button>
                <ul class="c-dropdown__menu" id="dropdown-i">
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Delete Anchor</button>
                  </li>
                  <li class="c-dropdown__item">
                    <button class="c-dropdown__button">Lock Anchor</button>
                  </li>
                </ul>
              </span>
            </li>
            <li class="c-action-list__actions">
              <button class="c-button c-button--primary">Some action</button>
            </li>
          </ul>

          ${warnBox({
            title: "Devices",
            message:
              "We recommend that you have at least two devices (for example, your computer and your phone).",
            additionalClasses: ["l-stack"],
            slot: html` <ul class="c-action-list">
              <li class="c-action-list__item">
                <span class="c-action-list__label">Single Device</span>
                <button class="c-action-list__action">${settingsIcon}</button>
              </li>
              <li class="c-action-list__actions">
                <button class="c-button c-button--primary">Some action</button>
              </li>
            </ul>`,
          })}
        </section>
        <p class="t-lead"></p>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title">Irregularities</h2>
        <p class="t-lead">
          Irregularities are messages that we show to the user when something
          goes wrong or something happens that we want to inform the user about.
          They can be used to show errors messages.
        </p>
        <section class="demo" aria-label="Irregularity Elements Demo">
          ${irregularity({
            message:
              "This is an error message. It can be used to inform the user about something that went wrong.",
            closeFn: () => {
              console.log("close");
            },
          })}
        </section>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Logo</h2>
        <section class="demo" aria-label="Logo Demo">
          <div class="c-logo">${icLogo}</div>
        </section>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title">Toast</h2>
        <p class="t-lead">
          Toasts are messages of varying length and importance that appear at
          the bottom or the top of the screen. They typically use the
          Irregularity component.
        </p>
        <section class="demo" aria-label="Toast Elements Demo">
          <button
            class="c-button c-button--primary"
            @click="${() =>
              toast.error(
                `some error message ${Math.round(Math.random() * 100)}`
              )}"
          >
            Show Toast
          </button>
        </section>
      </aside>

      <aside class="l-stack demo-section">
        <h2 class="t-title t-title--sub">Modal</h2>
        <section class="demo" aria-label="Modal Demo">
          <button
            class="c-button c-button--primary"
            @click=${async () =>
              await modal({
                slot: html`<h1>I am a modal</h1>`,
              })}
          >
            Open Modal
          </button>
        </section>
      </aside>
    </article>
  </section>
`;
