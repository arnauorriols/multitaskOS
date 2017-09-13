# Multitasking OS

Small app to delegate to the machine what it do best, and we do worst, when multitasking: Context switching.

From version 2.0.0, it's available publicly in the [Chrome Web Store](https://chrome.google.com/webstore/detail/multitaskos/ocdlpdmejajjjfcmhggnhbeacmgnabad).

It can also be used as a website at https://arnauorriols.github.io/multitaskOS

## Build

```
$ bin/build.sh
```


## Use as Chrome-Extension

1. Get everything ready

```
$ bin/build-chrome-extension.sh
```

2. Use the [Load the Extension Docs](https://developer.chrome.com/extensions/getstarted#unpacked) from
chrome documentation to manually load the application. The manifest to upload is found in `chrome-extension/manifest.json`.
