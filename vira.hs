-- Pipeline configuration for Vira
\ctx pipeline ->
  let isMain = ctx.branch == "main"
  in pipeline
    { signoff.enable = False
    , cachix.enable = True
    , attic.enable = isMain
    }
