# File size

A GitHub Action for recording file sizes in git notes.

## Usage

``` yaml
  record-readme-size:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: ./.github/actions/file-size
        with:
          file: README.md
```

To use the notes, first fetch them:

``` bash
$ git fetch origin refs/notes/file-size/*:refs/notes/file-size/*
```

Then, each note contains a number, which is the size in bytes. The following example plots the values:

``` bash
$ git log \
    --reverse --notes="notes/file-size/README.md" \
    --pretty='format:%h%x09%s%x09%N' | tail -n 10 \
    | awk -F '\t' 'NF { printf("%s\t%s\t", $1,$2); if ($3 != "") { printf("Size: %s ", $3); i = 0; while (i++ < ($3 - 2035000) / 200) printf "■" }; print "" }'

f3f0136 Add feature X             Size: 2039380 ■■■■■■■■■■■■■■■■■■■■■■
95495ba Fix bug                   Size: 2039380 ■■■■■■■■■■■■■■■■■■■■■■
5cbb463 Update logo               Size: 2039380 ■■■■■■■■■■■■■■■■■■■■■■
8a9f4aa Minify javascript         Size: 2038689 ■■■■■■■■■■■■■■■■■■■
da88075 Frobnicate with Aardvark  Size: 2045766 ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
...
```
