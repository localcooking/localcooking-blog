module Colors where

import MaterialUI.MuiThemeProvider (ColorPalette)


palette :: {primary :: ColorPalette, secondary :: ColorPalette}
palette =
  { primary:
    { light: "#d3b8ae"
    , main: "#a1887f"
    , dark: "#725b53"
    , contrastText: "#000000"
    }
  , secondary:
    { light: "#ffffff"
    , main: "#e0e0e0"
    , dark: "#aeaeae"
    , contrastText: "#000000"
    }
  }
