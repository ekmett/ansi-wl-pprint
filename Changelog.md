## 1.0.2

- Change the implementation to the `prettyprinter` package,
  using the `prettyprinter-compat-ansi-wl-pprint` compatibility package.

  Migrate to `prettyprinter`.

  Note: Using `show` won't preserve formatting anymore,
  as `prettyprinter`s `Show Doc` instance is annotation invariant.
  You can convert to `String` using
  `Data.Text.Lazy.unpack . Prettyprinter.Render.Terminal.renderLazy . Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions`,
  from the `text`, `prettyprinter-ansi-terminal`, `prettyprinter` packages respectively.

- The `Text.PrettyPrint.ANSI.Leijen.Internal` module is gone.

## 0.6.9

- All modules are explicitly `Safe`.
- Support GHC-7.0 ... GHC-8.8

## 0.6.8.2

