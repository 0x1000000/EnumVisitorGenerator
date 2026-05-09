1.1.2
- Fixed support for applying [VisitorToMethod] to private visitor structs declared inside matching partial enum extension class.
- Added DevelopmentDependency package metadata for source-generator tooling usage, improving analyzer-only package consumption behavior.
- Breaking note: if project A used the generator only through a reference to project B, project A may now need its own direct PackageReference to EnumVisitorGenerator. Each project that uses [VisitorGenerator] or [VisitorToMethod] should reference the package directly.
- For backward compatibility with transitive analyzer flow, projects can use: `<PackageReference Include="EnumVisitorGenerator" Version="1.1.2" IncludeAssets="analyzers" />`

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
