1.1.0
- Added `VisitorToMethodAttribute` for generating enum extension wrapper methods from stateless struct visitors.
- Added support for both `I{Enum}Visitor<TResult>` and `I{Enum}Visitor<TResult, TArg>`.
- Added tuple argument flattening for generated wrapper methods.
- Improved diagnostics for invalid visitor-to-method configurations and signature collisions.
- Generated `<EnumName>EnumExtension` as `partial`.
- Fixed test discovery by adding NUnit test adapter.

1.0.0 - Initial version
