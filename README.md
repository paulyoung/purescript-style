# purescript-style

[![Build Status](https://travis-ci.com/paulyoung/purescript-style.svg?branch=master)](https://travis-ci.com/paulyoung/purescript-style)

## Usage

```purescript
inline
  [ backgroundColor $ rgb 127 127 127
  , color black
  , fontSize $ 16.0 # px
  , height $ 100.0 # pct
  , margin zero
  , padding $ 2.0 # em
  , textAlign center
  , width auto
  ]
```

```
"background-color: rgb(127, 127, 127); color: rgb(0, 0, 0); font-size: 16px; height: 100%; margin: 0; padding: 2em; text-align: center; width: auto;"
```
