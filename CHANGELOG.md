1.1.1
- Added cross-project `VisitorToMethod` generation so a consuming assembly can emit its own enum extension wrapper for referenced public enums.
- Added diagnostics for applying `[VisitorGenerator]` to private enums and `[VisitorToMethod]` to private structs.
- Improved validation around unsupported private targets.

1.1.0
- Added `VisitorToMethodAttribute` for generating enum extension wrapper methods from stateless struct visitors.
- Added support for both `I{Enum}Visitor<TResult>` and `I{Enum}Visitor<TResult, TArg>`.
- Added tuple argument flattening for generated wrapper methods.
- Improved diagnostics for invalid visitor-to-method configurations and signature collisions.
- Generated `<EnumName>EnumExtension` as `partial`.
- Fixed test discovery by adding NUnit test adapter.

1.0.0 - Initial version
