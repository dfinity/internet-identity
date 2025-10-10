# Localizing

This document explains how to localize Internet Identity into other languages.

## Pre-requisites

Make sure Node.js has been installed and this project dependencies have been installed:

```bash
npm ci
```

## Extracting

First update the locale catalogs with the latest changes from the frontend implementation:

```bash
npm run extract
```

## Catalogs

The localization files can be found and directly edited at `/src/frontend/src/lib/locales/{locale}.po`.

See [HACKING](./HACKING.md) for instructions to run II locally to see the locale changes.