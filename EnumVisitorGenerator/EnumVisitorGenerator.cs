using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EnumVisitorGenerator;

[Generator]
public class EnumVisitorGenerator : IIncrementalGenerator
{
    private const string VisitorGeneratorAttributeFullName = "EnumVisitorGenerator.VisitorGeneratorAttribute";
    private const string VisitorToMethodAttributeFullName = "EnumVisitorGenerator.VisitorToMethodAttribute";
    private const string VisitorInterfaceSuffix = "Visitor";

    private static readonly SymbolDisplayFormat TypeDisplayFormat = new(
        globalNamespaceStyle: SymbolDisplayGlobalNamespaceStyle.Included,
        typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
        genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters,
        miscellaneousOptions: SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers
                              | SymbolDisplayMiscellaneousOptions.UseSpecialTypes
                              | SymbolDisplayMiscellaneousOptions.IncludeNullableReferenceTypeModifier
    );

    private record struct EnumSemanticTarget(EnumDeclarationSyntax Enum, AttributeSyntax Attribute, AttributeSyntax? Attribute2);
    private record struct VisitorMethodSemanticTarget(StructDeclarationSyntax Struct, AttributeSyntax Attribute, AttributeSyntax? Attribute2);

    private sealed class EnumGenerationContext(
        EnumDeclarationSyntax enumDeclaration,
        BaseNamespaceDeclarationSyntax namespaceSyntax,
        string namespaceName,
        string enumName,
        string hintName,
        bool isPublic,
        AttributeSyntax attribute)
    {
        public EnumDeclarationSyntax EnumDeclaration { get; } = enumDeclaration;
        public BaseNamespaceDeclarationSyntax NamespaceSyntax { get; } = namespaceSyntax;
        public string NamespaceName { get; } = namespaceName;
        public string EnumName { get; } = enumName;
        public string HintName { get; } = hintName;
        public bool IsPublic { get; } = isPublic;
        public AttributeSyntax Attribute { get; } = attribute;
        public List<MemberDeclarationSyntax> VisitorMethods { get; } = new();
        public HashSet<string> VisitorMethodSignatures { get; } = new(StringComparer.Ordinal);
    }

    private sealed class VisitorExtensionGenerationContext(
        string enumName,
        string namespaceName,
        bool hasNamespace,
        bool isPublic,
        string hintName)
    {
        public string EnumName { get; } = enumName;
        public string NamespaceName { get; } = namespaceName;
        public bool HasNamespace { get; } = hasNamespace;
        public bool IsPublic { get; } = isPublic;
        public string HintName { get; } = hintName;
        public List<MemberDeclarationSyntax> Methods { get; } = new();
        public HashSet<string> MethodSignatures { get; } = new(StringComparer.Ordinal);
    }

    private record struct VisitorMethodBinding(
        EnumGenerationContext? EnumContext,
        string EnumName,
        string EnumNamespaceName,
        bool EnumHasNamespace,
        bool EnumIsPublic,
        string VisitorTypeName,
        string ResultTypeName,
        bool HasArgument,
        string? ArgTypeName,
        ITypeSymbol? ArgTypeSymbol,
        TypeSyntax? ArgTypeSyntax);

    private record struct TupleArgParameter(string TypeName, string Name);

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        var enumDeclarations = context.SyntaxProvider.CreateSyntaxProvider(
                predicate: static (syntaxNode, _) => syntaxNode is EnumDeclarationSyntax
                {
                    AttributeLists.Count: > 0, Members.Count: > 0
                },
                transform: GetEnumSemanticTargetForGeneration
            )
            .Where(static x => !ReferenceEquals(x.Enum, null));

        var visitorMethodStructs = context.SyntaxProvider.CreateSyntaxProvider(
                predicate: static (syntaxNode, _) => syntaxNode is StructDeclarationSyntax
                {
                    AttributeLists.Count: > 0
                },
                transform: GetVisitorMethodSemanticTargetForGeneration
            )
            .Where(static x => !ReferenceEquals(x.Struct, null));

        context.RegisterPostInitializationOutput(
            ctx =>
            {
                ctx.AddSource(
                    "VisitorGeneratorAttribute.g.cs",
                    "namespace EnumVisitorGenerator { [global::System.AttributeUsage(global::System.AttributeTargets.Enum)][global::System.Diagnostics.Conditional(\"ENUM_VISITOR_GENERATOR_USAGES\")] internal class VisitorGeneratorAttribute : global::System.Attribute { } }"
                );
                ctx.AddSource(
                    "VisitorToMethodAttribute.g.cs",
                    "namespace EnumVisitorGenerator { [global::System.AttributeUsage(global::System.AttributeTargets.Struct)][global::System.Diagnostics.Conditional(\"ENUM_VISITOR_GENERATOR_USAGES\")] internal class VisitorToMethodAttribute : global::System.Attribute { public VisitorToMethodAttribute(global::System.String methodName) { } } }"
                );
            }
        );

        var generationContext = context.CompilationProvider.Combine(enumDeclarations.Collect()).Combine(visitorMethodStructs.Collect());

        context.RegisterSourceOutput(generationContext, Execute);
    }

    private static EnumSemanticTarget GetEnumSemanticTargetForGeneration(GeneratorSyntaxContext ctx, CancellationToken ct)
    {
        if (ctx.Node is EnumDeclarationSyntax enumDeclarationSyntax)
        {
            AttributeSyntax? att1 = null;
            AttributeSyntax? att2 = null;

            foreach (var attributeListSyntax in enumDeclarationSyntax.AttributeLists)
            {
                foreach (var attributeSyntax in attributeListSyntax.Attributes)
                {
                    var symbolInfo = ctx.SemanticModel.GetSymbolInfo(attributeSyntax, ct);
                    if (symbolInfo.Symbol is not IMethodSymbol attributeSymbol)
                    {
                        continue;
                    }

                    var attributeContainingTypeSymbol = attributeSymbol.ContainingType;
                    var fullName = attributeContainingTypeSymbol.ToDisplayString();

                    if (fullName == VisitorGeneratorAttributeFullName)
                    {
                        if (att1 == null)
                        {
                            att1 = attributeSyntax;
                        }
                        else
                        {
                            if (att2 == null)
                            {
                                att2 = attributeSyntax;
                            }
                            else
                            {
                                return new(enumDeclarationSyntax, att1, att2);
                            }
                        }
                    }
                }
            }

            if (att1 == null)
            {
                return new(null!, att1!, att2!);
            }

            return new(enumDeclarationSyntax, att1, att2);
        }

        return new(null!, null!, null!);
    }

    private static VisitorMethodSemanticTarget GetVisitorMethodSemanticTargetForGeneration(GeneratorSyntaxContext ctx, CancellationToken ct)
    {
        if (ctx.Node is StructDeclarationSyntax structDeclarationSyntax)
        {
            AttributeSyntax? att1 = null;
            AttributeSyntax? att2 = null;

            foreach (var attributeListSyntax in structDeclarationSyntax.AttributeLists)
            {
                foreach (var attributeSyntax in attributeListSyntax.Attributes)
                {
                    var symbolInfo = ctx.SemanticModel.GetSymbolInfo(attributeSyntax, ct);
                    if (symbolInfo.Symbol is not IMethodSymbol attributeSymbol)
                    {
                        continue;
                    }

                    var attributeContainingTypeSymbol = attributeSymbol.ContainingType;
                    var fullName = attributeContainingTypeSymbol.ToDisplayString();

                    if (fullName == VisitorToMethodAttributeFullName)
                    {
                        if (att1 == null)
                        {
                            att1 = attributeSyntax;
                        }
                        else
                        {
                            if (att2 == null)
                            {
                                att2 = attributeSyntax;
                            }
                            else
                            {
                                return new(structDeclarationSyntax, att1, att2);
                            }
                        }
                    }
                }
            }

            if (att1 == null)
            {
                return new(null!, att1!, att2!);
            }

            return new(structDeclarationSyntax, att1, att2);
        }

        return new(null!, null!, null!);
    }

    private static void Execute(
        SourceProductionContext ctx,
        ((Compilation Left, ImmutableArray<EnumSemanticTarget> Right) Left, ImmutableArray<VisitorMethodSemanticTarget> Right) source)
    {
        var compilation = source.Left.Left;
        var enumTargets = source.Left.Right;
        var visitorTargets = source.Right;

        if (enumTargets.IsDefaultOrEmpty && visitorTargets.IsDefaultOrEmpty)
        {
            return;
        }

        var duplicateDetector = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase);
        var enumGenerationContexts = new List<EnumGenerationContext>(enumTargets.Length);

        foreach (var semanticTarget in enumTargets)
        {
            if (semanticTarget.Attribute2 != null)
            {
                ctx.ReportDiagnostic(
                    Diagnostic.Create(
                        new DiagnosticDescriptor(
                            "EG0002",
                            "VisitorGeneratorAttribute declaration",
                            "VisitorGeneratorAttribute has been declared more than one time",
                            "Enum Visitor Generator",
                            DiagnosticSeverity.Error,
                            true
                        ),
                        semanticTarget.Attribute2.GetLocation()
                    )
                );
            }

            var enumDeclarationSyntax = semanticTarget.Enum;
            var enumSemanticModel = compilation.GetSemanticModel(enumDeclarationSyntax.SyntaxTree);
            var enumSymbol = enumSemanticModel.GetDeclaredSymbol(enumDeclarationSyntax);
            if (enumSymbol?.DeclaredAccessibility == Accessibility.Private)
            {
                ctx.ReportDiagnostic(
                    Diagnostic.Create(
                        new DiagnosticDescriptor(
                            "EG0010",
                            "VisitorGeneratorAttribute declaration",
                            "VisitorGeneratorAttribute cannot be applied to a private enum",
                            "Enum Visitor Generator",
                            DiagnosticSeverity.Error,
                            true
                        ),
                        semanticTarget.Attribute.GetLocation()
                    )
                );
                continue;
            }

            var namespaceSyntax = GetNamespace(enumDeclarationSyntax, out var parentTypeSyntax);
            if (namespaceSyntax == null)
            {
                continue;
            }

            if (parentTypeSyntax != null)
            {
                ctx.ReportDiagnostic(
                    Diagnostic.Create(
                        new DiagnosticDescriptor(
                            "EG0003",
                            "VisitorGeneratorAttribute declaration for a nested enum",
                            "VisitorGeneratorAttribute cannot be applied to a nested enum",
                            "Enum Visitor Generator",
                            DiagnosticSeverity.Error,
                            true
                        ),
                        semanticTarget.Attribute.GetLocation()
                    )
                );

                continue;
            }

            var enumName = enumDeclarationSyntax.Identifier.Text;
            var helperClassName = $"{enumName}EnumExtension";
            var hintName = $"{namespaceSyntax.Name}.{helperClassName}.cs";

            if (!duplicateDetector.Add(hintName))
            {
                ctx.ReportDiagnostic(
                    Diagnostic.Create(
                        new DiagnosticDescriptor(
                            "EG0001",
                            "Enum generation name conflict",
                            "Enum generation with the same name already exists",
                            "Enum Visitor Generator",
                            DiagnosticSeverity.Error,
                            true
                        ),
                        semanticTarget.Attribute.GetLocation()
                    )
                );
                continue;
            }

            enumGenerationContexts.Add(
                new EnumGenerationContext(
                    enumDeclarationSyntax,
                    namespaceSyntax,
                    namespaceSyntax.Name.ToString(),
                    enumName,
                    hintName,
                    enumDeclarationSyntax.Modifiers.Any(t => t.IsKind(SyntaxKind.PublicKeyword)),
                    semanticTarget.Attribute
                )
            );
        }

        var enumByNameAndNamespace = enumGenerationContexts.ToDictionary(
            c => GetEnumKey(c.NamespaceName, c.EnumName),
            c => c,
            StringComparer.Ordinal
        );
        var enumByName = enumGenerationContexts.GroupBy(c => c.EnumName, StringComparer.Ordinal)
            .ToDictionary(g => g.Key, g => g.ToList(), StringComparer.Ordinal);
        var externalVisitorExtensions = new Dictionary<string, VisitorExtensionGenerationContext>(StringComparer.Ordinal);

        if (!visitorTargets.IsDefaultOrEmpty)
        {
            foreach (var visitorTarget in visitorTargets)
            {
                if (visitorTarget.Attribute2 != null)
                {
                    ctx.ReportDiagnostic(
                        Diagnostic.Create(
                            new DiagnosticDescriptor(
                                "EG0004",
                                "VisitorToMethodAttribute declaration",
                                "VisitorToMethodAttribute has been declared more than one time",
                                "Enum Visitor Generator",
                                DiagnosticSeverity.Error,
                                true
                            ),
                            visitorTarget.Attribute2.GetLocation()
                        )
                    );
                }

                var semanticModel = compilation.GetSemanticModel(visitorTarget.Struct.SyntaxTree);
                var methodName = GetMethodName(visitorTarget.Attribute, semanticModel);
                if (string.IsNullOrWhiteSpace(methodName) || !SyntaxFacts.IsValidIdentifier(methodName))
                {
                    ctx.ReportDiagnostic(
                        Diagnostic.Create(
                            new DiagnosticDescriptor(
                                "EG0006",
                                "VisitorToMethodAttribute declaration",
                                "VisitorToMethodAttribute method name must be a valid C# identifier",
                                "Enum Visitor Generator",
                                DiagnosticSeverity.Error,
                                true
                            ),
                            visitorTarget.Attribute.GetLocation()
                        )
                    );
                    continue;
                }

                var structSymbol = semanticModel.GetDeclaredSymbol(visitorTarget.Struct);
                if (structSymbol is not INamedTypeSymbol visitorStructSymbol)
                {
                    continue;
                }

                if (!IsStatelessStruct(visitorStructSymbol))
                {
                    ctx.ReportDiagnostic(
                        Diagnostic.Create(
                            new DiagnosticDescriptor(
                                "EG0007",
                                "VisitorToMethodAttribute declaration",
                                "VisitorToMethodAttribute can only be applied to stateless structs without instance fields/properties/events or constructors",
                                "Enum Visitor Generator",
                                DiagnosticSeverity.Error,
                                true
                            ),
                            visitorTarget.Attribute.GetLocation()
                        )
                    );
                    continue;
                }

                var bindings = GetVisitorMethodBindings(
                    visitorTarget.Struct,
                    semanticModel,
                    visitorStructSymbol,
                    enumByNameAndNamespace,
                    enumByName
                );

                if (bindings.Count == 0)
                {
                    ctx.ReportDiagnostic(
                        Diagnostic.Create(
                            new DiagnosticDescriptor(
                                "EG0005",
                                "VisitorToMethodAttribute declaration",
                                "VisitorToMethodAttribute struct must implement a generated I{Enum}Visitor<TResult> or I{Enum}Visitor<TResult, TArg> interface",
                                "Enum Visitor Generator",
                                DiagnosticSeverity.Error,
                                true
                            ),
                            visitorTarget.Attribute.GetLocation()
                        )
                    );
                    continue;
                }

                if (bindings.Count > 1)
                {
                    ctx.ReportDiagnostic(
                        Diagnostic.Create(
                            new DiagnosticDescriptor(
                                "EG0009",
                                "VisitorToMethodAttribute declaration",
                                "VisitorToMethodAttribute struct must implement only one generated I{Enum}Visitor<TResult> or I{Enum}Visitor<TResult, TArg> interface",
                                "Enum Visitor Generator",
                                DiagnosticSeverity.Error,
                                true
                            ),
                            visitorTarget.Attribute.GetLocation()
                        )
                    );
                    continue;
                }

                var binding = bindings[0];
                if (visitorStructSymbol.DeclaredAccessibility == Accessibility.Private &&
                    !IsPrivateVisitorInsideMatchingEnumExtension(visitorStructSymbol, binding))
                {
                    ctx.ReportDiagnostic(
                        Diagnostic.Create(
                            new DiagnosticDescriptor(
                                "EG0011",
                                "VisitorToMethodAttribute declaration",
                                "VisitorToMethodAttribute can be applied to a private struct only when it is declared inside matching {Enum}EnumExtension partial class",
                                "Enum Visitor Generator",
                                DiagnosticSeverity.Error,
                                true
                            ),
                            visitorTarget.Attribute.GetLocation()
                        )
                    );
                    continue;
                }

                var signatureKey = BuildVisitorMethodSignature(
                    methodName,
                    binding.HasArgument,
                    binding.ArgTypeName,
                    binding.ArgTypeSymbol,
                    binding.ArgTypeSyntax
                );

                if (binding.EnumContext != null)
                {
                    if (!binding.EnumContext.VisitorMethodSignatures.Add(signatureKey))
                    {
                        ctx.ReportDiagnostic(
                            Diagnostic.Create(
                                new DiagnosticDescriptor(
                                    "EG0008",
                                    "VisitorToMethod method collision",
                                    "VisitorToMethodAttribute method name with the same arguments has already been used for this enum (overloads with different arguments are allowed)",
                                    "Enum Visitor Generator",
                                    DiagnosticSeverity.Error,
                                    true
                                ),
                                visitorTarget.Attribute.GetLocation()
                            )
                        );
                        continue;
                    }

                    binding.EnumContext.VisitorMethods.Add(
                        GenerateVisitorToMethod(
                            binding.EnumContext.IsPublic,
                            binding.EnumContext.EnumName,
                            methodName,
                            binding.VisitorTypeName,
                            binding.ResultTypeName,
                            binding.HasArgument,
                            binding.ArgTypeName,
                            binding.ArgTypeSymbol,
                            binding.ArgTypeSyntax
                        )
                    );
                    continue;
                }

                var extensionKey = GetEnumKey(binding.EnumNamespaceName, binding.EnumName);
                if (!externalVisitorExtensions.TryGetValue(extensionKey, out var extensionContext))
                {
                    extensionContext = new VisitorExtensionGenerationContext(
                        binding.EnumName,
                        binding.EnumNamespaceName,
                        binding.EnumHasNamespace,
                        binding.EnumIsPublic,
                        CreateVisitorExtensionHintName(binding.EnumNamespaceName, binding.EnumHasNamespace, binding.EnumName)
                    );
                    externalVisitorExtensions.Add(extensionKey, extensionContext);
                }

                if (!extensionContext.MethodSignatures.Add(signatureKey))
                {
                    ctx.ReportDiagnostic(
                        Diagnostic.Create(
                            new DiagnosticDescriptor(
                                "EG0008",
                                "VisitorToMethod method collision",
                                "VisitorToMethodAttribute method name with the same arguments has already been used for this enum (overloads with different arguments are allowed)",
                                "Enum Visitor Generator",
                                DiagnosticSeverity.Error,
                                true
                            ),
                            visitorTarget.Attribute.GetLocation()
                        )
                    );
                    continue;
                }

                extensionContext.Methods.Add(
                    GenerateVisitorToMethod(
                        extensionContext.IsPublic,
                        extensionContext.EnumName,
                        methodName,
                        binding.VisitorTypeName,
                        binding.ResultTypeName,
                        binding.HasArgument,
                        binding.ArgTypeName,
                        binding.ArgTypeSymbol,
                        binding.ArgTypeSyntax
                    )
                );
            }
        }

        foreach (var enumContext in enumGenerationContexts)
        {
            var enumDeclarationSyntax = enumContext.EnumDeclaration;
            var members = enumDeclarationSyntax.Members.Select(m => m.Identifier.Text).ToList();
            var helperMembers = new List<MemberDeclarationSyntax>
            {
                GenerateVoidAccept(enumContext.IsPublic, enumContext.EnumName, members),
                GenerateVoidStructAccept(enumContext.IsPublic, enumContext.EnumName, members),
                GenerateGenericResultAccept(enumContext.IsPublic, false, enumContext.EnumName, members),
                GenerateGenericResultStructAccept(enumContext.IsPublic, false, enumContext.EnumName, members),
                GenerateGenericResultAccept(enumContext.IsPublic, true, enumContext.EnumName, members),
                GenerateGenericResultStructAccept(enumContext.IsPublic, true, enumContext.EnumName, members)
            };
            helperMembers.AddRange(enumContext.VisitorMethods);

            var rootNameSpace = enumContext.NamespaceSyntax.WithMembers(
                List(
                    new[]
                    {
                        ClassDeclaration($"{enumContext.EnumName}EnumExtension")
                            .WithModifiers(
                                TokenList(
                                    Token(enumContext.IsPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword),
                                    Token(SyntaxKind.StaticKeyword),
                                    Token(SyntaxKind.PartialKeyword)
                                )
                            )
                            .WithMembers(
                                List(helperMembers)
                            ),
                        GenerateVoidInterface(enumContext.IsPublic, enumContext.EnumName, members),
                        GenerateGenericResultInterface(enumContext.IsPublic, enumContext.EnumName, members),
                        GenerateGenericResultInterfaceArgs(enumContext.IsPublic, enumContext.EnumName, members)
                    }
                )
            );

            var compilationUnitSyntax = CompilationUnit()
                .WithUsings(SingletonList(UsingDirective(IdentifierName("System"))))
                .WithMembers(SingletonList<MemberDeclarationSyntax>(rootNameSpace))
                .NormalizeWhitespace();

            ctx.AddSource(enumContext.HintName, compilationUnitSyntax.ToFullString());
        }

        foreach (var extensionContext in externalVisitorExtensions.Values)
        {
            var compilationUnitSyntax = CompilationUnit()
                .WithMembers(SingletonList(GenerateVisitorExtensionRoot(extensionContext)))
                .NormalizeWhitespace();

            ctx.AddSource(extensionContext.HintName, compilationUnitSyntax.ToFullString());
        }
    }

    private static string GetEnumKey(string namespaceName, string enumName) => $"{NormalizeNamespace(namespaceName)}|{enumName}";

    private static string NormalizeNamespace(string value) =>
        value.StartsWith("global::", StringComparison.Ordinal)
            ? value.Substring("global::".Length)
            : value;

    private static string CreateVisitorExtensionHintName(string namespaceName, bool hasNamespace, string enumName) =>
        hasNamespace
            ? $"{namespaceName}.{enumName}EnumExtension.VisitorToMethod.cs"
            : $"{enumName}EnumExtension.VisitorToMethod.cs";

    private static MemberDeclarationSyntax GenerateVisitorExtensionRoot(VisitorExtensionGenerationContext extensionContext)
    {
        var classDeclaration = ClassDeclaration($"{extensionContext.EnumName}EnumExtension")
            .WithModifiers(
                TokenList(
                    Token(extensionContext.IsPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword),
                    Token(SyntaxKind.StaticKeyword),
                    Token(SyntaxKind.PartialKeyword)
                )
            )
            .WithMembers(List(extensionContext.Methods));

        return extensionContext.HasNamespace
            ? NamespaceDeclaration(ParseName(extensionContext.NamespaceName))
                .WithMembers(SingletonList<MemberDeclarationSyntax>(classDeclaration))
            : classDeclaration;
    }

    private static string? GetMethodName(AttributeSyntax attributeSyntax, SemanticModel semanticModel)
    {
        var arguments = attributeSyntax.ArgumentList?.Arguments;
        if (arguments == null || arguments.Value.Count != 1)
        {
            return null;
        }

        var constant = semanticModel.GetConstantValue(arguments.Value[0].Expression);
        return constant is { HasValue: true, Value: string name } ? name : null;
    }

    private static bool IsStatelessStruct(INamedTypeSymbol structSymbol)
    {
        if (structSymbol.InstanceConstructors.Any(c => !c.IsImplicitlyDeclared))
        {
            return false;
        }

        if (structSymbol.GetMembers().OfType<IFieldSymbol>().Any(f => !f.IsStatic))
        {
            return false;
        }

        if (structSymbol.GetMembers().OfType<IPropertySymbol>().Any(p => !p.IsStatic))
        {
            return false;
        }

        if (structSymbol.GetMembers().OfType<IEventSymbol>().Any(e => !e.IsStatic))
        {
            return false;
        }

        return true;
    }

    private static bool IsPrivateVisitorInsideMatchingEnumExtension(INamedTypeSymbol visitorStructSymbol, VisitorMethodBinding binding)
    {
        var container = visitorStructSymbol.ContainingType;
        if (container == null)
        {
            return false;
        }

        if (!string.Equals(container.Name, $"{binding.EnumName}EnumExtension", StringComparison.Ordinal))
        {
            return false;
        }

        var containerNamespace = container.ContainingNamespace;
        var containerNamespaceName = containerNamespace.IsGlobalNamespace ? string.Empty : containerNamespace.ToDisplayString();
        return string.Equals(containerNamespaceName, NormalizeNamespace(binding.EnumNamespaceName), StringComparison.Ordinal);
    }

    private static List<VisitorMethodBinding> GetVisitorMethodBindings(
        StructDeclarationSyntax structDeclarationSyntax,
        SemanticModel semanticModel,
        INamedTypeSymbol structSymbol,
        IReadOnlyDictionary<string, EnumGenerationContext> enumByNameAndNamespace,
        IReadOnlyDictionary<string, List<EnumGenerationContext>> enumByName)
    {
        var bindings = new List<VisitorMethodBinding>();
        var namespaceSyntax = GetNamespace(structDeclarationSyntax, out _);
        var defaultNamespace = namespaceSyntax?.Name.ToString();
        var visitorTypeName = structSymbol.ToDisplayString(TypeDisplayFormat);

        if (structDeclarationSyntax.BaseList == null)
        {
            return bindings;
        }

        foreach (var baseTypeSyntax in structDeclarationSyntax.BaseList.Types)
        {
            if (!TryMatchVisitorInterface(
                    baseTypeSyntax.Type,
                    out var enumName,
                    out var resultTypeSyntax,
                    out var hasArgument,
                    out var argTypeSyntax,
                    out var namespaceHint))
            {
                continue;
            }

            var enumContext = ResolveEnumContext(
                enumName,
                namespaceHint,
                defaultNamespace,
                enumByNameAndNamespace,
                enumByName);

            string enumNamespaceName;
            bool enumHasNamespace;
            bool enumIsPublic;

            if (enumContext != null)
            {
                enumNamespaceName = enumContext.NamespaceName;
                enumHasNamespace = true;
                enumIsPublic = enumContext.IsPublic;
            }
            else
            {
                var interfaceSymbol = semanticModel.GetTypeInfo(baseTypeSyntax.Type).Type as INamedTypeSymbol;
                if (!TryResolveReferencedEnum(interfaceSymbol, enumName, out enumNamespaceName, out enumHasNamespace, out enumIsPublic))
                {
                    continue;
                }
            }

            var resultTypeName = GetTypeName(resultTypeSyntax, semanticModel);
            string? argTypeName = null;
            ITypeSymbol? argTypeSymbol = null;
            if (hasArgument && argTypeSyntax != null)
            {
                argTypeName = GetTypeName(argTypeSyntax, semanticModel);
                argTypeSymbol = semanticModel.GetTypeInfo(argTypeSyntax).Type;
            }

            bindings.Add(
                new VisitorMethodBinding(
                    enumContext,
                    enumName,
                    enumNamespaceName,
                    enumHasNamespace,
                    enumIsPublic,
                    visitorTypeName,
                    resultTypeName,
                    hasArgument,
                    argTypeName,
                    argTypeSymbol,
                    argTypeSyntax
                )
            );
        }

        return bindings;
    }

    private static bool TryResolveReferencedEnum(
        INamedTypeSymbol? interfaceSymbol,
        string enumName,
        out string namespaceName,
        out bool hasNamespace,
        out bool isPublic)
    {
        namespaceName = string.Empty;
        hasNamespace = false;
        isPublic = false;

        if (interfaceSymbol == null)
        {
            return false;
        }

        var containingNamespace = interfaceSymbol.ContainingNamespace;
        var enumSymbol = containingNamespace.GetTypeMembers(enumName).FirstOrDefault(t => t.TypeKind == TypeKind.Enum);
        if (enumSymbol == null)
        {
            return false;
        }

        hasNamespace = !containingNamespace.IsGlobalNamespace;
        namespaceName = hasNamespace ? containingNamespace.ToDisplayString() : string.Empty;
        isPublic = enumSymbol.DeclaredAccessibility == Accessibility.Public;
        return true;
    }

    private static string GetTypeName(TypeSyntax typeSyntax, SemanticModel semanticModel)
    {
        var symbol = semanticModel.GetTypeInfo(typeSyntax).Type;
        if (symbol == null || symbol.TypeKind == TypeKind.Error)
        {
            return typeSyntax.ToString();
        }

        return symbol.ToDisplayString(TypeDisplayFormat);
    }

    private static EnumGenerationContext? ResolveEnumContext(
        string enumName,
        string? namespaceHint,
        string? defaultNamespace,
        IReadOnlyDictionary<string, EnumGenerationContext> enumByNameAndNamespace,
        IReadOnlyDictionary<string, List<EnumGenerationContext>> enumByName)
    {
        if (namespaceHint is { Length: > 0 } namespaceHintValue &&
            enumByNameAndNamespace.TryGetValue(GetEnumKey(namespaceHintValue, enumName), out var byNamespaceHint))
        {
            return byNamespaceHint;
        }

        if (defaultNamespace is { Length: > 0 } defaultNamespaceValue &&
            enumByNameAndNamespace.TryGetValue(GetEnumKey(defaultNamespaceValue, enumName), out var byDefaultNamespace))
        {
            return byDefaultNamespace;
        }

        return enumByName.TryGetValue(enumName, out var all) && all.Count == 1 ? all[0] : null;
    }

    private static bool TryMatchVisitorInterface(
        TypeSyntax typeSyntax,
        out string enumName,
        out TypeSyntax resultTypeSyntax,
        out bool hasArgument,
        out TypeSyntax? argTypeSyntax,
        out string? namespaceHint)
    {
        enumName = null!;
        resultTypeSyntax = null!;
        hasArgument = false;
        argTypeSyntax = null;
        namespaceHint = null;

        GenericNameSyntax? genericName = null;

        switch (typeSyntax)
        {
            case GenericNameSyntax plainGenericName:
                genericName = plainGenericName;
                break;
            case QualifiedNameSyntax qualifiedNameSyntax when qualifiedNameSyntax.Right is GenericNameSyntax qualifiedGenericName:
                genericName = qualifiedGenericName;
                namespaceHint = qualifiedNameSyntax.Left.ToString();
                break;
            case AliasQualifiedNameSyntax aliasQualifiedNameSyntax when aliasQualifiedNameSyntax.Name is GenericNameSyntax aliasGenericName:
                genericName = aliasGenericName;
                namespaceHint = aliasQualifiedNameSyntax.Alias.ToString();
                break;
        }

        if (genericName == null)
        {
            return false;
        }

        var genericArgCount = genericName.TypeArgumentList.Arguments.Count;
        if (genericArgCount != 1 && genericArgCount != 2)
        {
            return false;
        }

        var visitorInterfaceName = genericName.Identifier.Text;
        if (!TryExtractEnumNameFromVisitorInterface(visitorInterfaceName, out enumName))
        {
            return false;
        }

        resultTypeSyntax = genericName.TypeArgumentList.Arguments[0];
        if (genericArgCount == 2)
        {
            hasArgument = true;
            argTypeSyntax = genericName.TypeArgumentList.Arguments[1];
        }

        return true;
    }

    private static bool TryExtractEnumNameFromVisitorInterface(string visitorInterfaceName, out string enumName)
    {
        enumName = string.Empty;

        if (!visitorInterfaceName.StartsWith("I", StringComparison.Ordinal) ||
            !visitorInterfaceName.EndsWith(VisitorInterfaceSuffix, StringComparison.Ordinal))
        {
            return false;
        }

        var enumNameLength = visitorInterfaceName.Length - 1 - VisitorInterfaceSuffix.Length;
        if (enumNameLength <= 0)
        {
            return false;
        }

        enumName = visitorInterfaceName.Substring(1, enumNameLength);
        return true;
    }

    private static BaseNamespaceDeclarationSyntax? GetNamespace(BaseTypeDeclarationSyntax syntax, out TypeDeclarationSyntax? parentType)
    {
        parentType = null;

        var potentialNamespaceParent = syntax.Parent;

        while (potentialNamespaceParent != null &&
               potentialNamespaceParent is not NamespaceDeclarationSyntax
               && potentialNamespaceParent is not FileScopedNamespaceDeclarationSyntax)
        {
            if (parentType == null && potentialNamespaceParent is TypeDeclarationSyntax typeDeclarationSyntax)
            {
                parentType = typeDeclarationSyntax;
            }
            potentialNamespaceParent = potentialNamespaceParent.Parent;
        }

        if (potentialNamespaceParent is BaseNamespaceDeclarationSyntax namespaceParent)
        {
            return namespaceParent;
        }

        return null;
    }

    private static string BuildVisitorMethodSignature(
        string methodName,
        bool hasArgument,
        string? argTypeName,
        ITypeSymbol? argTypeSymbol,
        TypeSyntax? argTypeSyntax)
    {
        if (!hasArgument)
        {
            return $"{methodName}|<no-arg>";
        }

        if (argTypeName == null || argTypeSyntax == null || !TryGetTupleParameters(argTypeSymbol, argTypeSyntax, out var tupleParameters))
        {
            return $"{methodName}|{argTypeName}";
        }

        return $"{methodName}|{string.Join("|", tupleParameters.Select(p => p.TypeName))}";
    }

    private static MemberDeclarationSyntax GenerateVisitorToMethod(
        bool isPublic,
        string enumName,
        string methodName,
        string visitorTypeName,
        string resultTypeName,
        bool hasArgument,
        string? argTypeName,
        ITypeSymbol? argTypeSymbol,
        TypeSyntax? argTypeSyntax)
    {
        var parameters = new List<SyntaxNodeOrToken>
        {
            Parameter(
                    Identifier("source")
                )
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.ThisKeyword)
                    )
                )
                .WithType(
                    IdentifierName(enumName)
                )
        };

        ExpressionSyntax? argExpression = null;

        if (hasArgument && argTypeSyntax != null && TryGetTupleParameters(argTypeSymbol, argTypeSyntax, out var tupleParameters))
        {
            foreach (var tupleParameter in tupleParameters)
            {
                parameters.Add(Token(SyntaxKind.CommaToken));
                parameters.Add(
                    Parameter(Identifier(tupleParameter.Name))
                        .WithType(ParseTypeName(tupleParameter.TypeName))
                );
            }

            var tupleArguments = new List<SyntaxNodeOrToken>(tupleParameters.Count * 2 - 1);
            for (int i = 0; i < tupleParameters.Count; i++)
            {
                if (i > 0)
                {
                    tupleArguments.Add(Token(SyntaxKind.CommaToken));
                }

                tupleArguments.Add(
                    Argument(
                        IdentifierName(tupleParameters[i].Name)
                    )
                );
            }

            argExpression = TupleExpression(
                SeparatedList<ArgumentSyntax>(tupleArguments)
            );
        }
        else if (hasArgument)
        {
            parameters.Add(Token(SyntaxKind.CommaToken));
            parameters.Add(
                Parameter(
                        Identifier("arg")
                    )
                    .WithType(
                        ParseTypeName(argTypeName!)
                    )
            );
            argExpression = IdentifierName("arg");
        }

        var acceptTypeArguments = hasArgument
            ? SeparatedList<TypeSyntax>(
                new SyntaxNodeOrToken[]
                {
                    ParseTypeName(resultTypeName),
                    Token(SyntaxKind.CommaToken),
                    ParseTypeName(visitorTypeName),
                    Token(SyntaxKind.CommaToken),
                    ParseTypeName(argTypeName!)
                }
            )
            : SeparatedList<TypeSyntax>(
                new SyntaxNodeOrToken[]
                {
                    ParseTypeName(resultTypeName),
                    Token(SyntaxKind.CommaToken),
                    ParseTypeName(visitorTypeName)
                }
            );

        var acceptArguments = hasArgument
            ? SeparatedList<ArgumentSyntax>(
                new SyntaxNodeOrToken[]
                {
                    Argument(
                            IdentifierName("visitor")
                        )
                        .WithRefKindKeyword(
                            Token(SyntaxKind.RefKeyword)
                        ),
                    Token(SyntaxKind.CommaToken),
                    Argument(argExpression!)
                }
            )
            : SingletonSeparatedList(
                Argument(
                        IdentifierName("visitor")
                    )
                    .WithRefKindKeyword(
                        Token(SyntaxKind.RefKeyword)
                    )
            );

        return MethodDeclaration(
                ParseTypeName(resultTypeName),
                Identifier(methodName)
            )
            .WithModifiers(
                TokenList(
                    Token(isPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword),
                    Token(SyntaxKind.StaticKeyword)
                )
            )
            .WithParameterList(
                ParameterList(
                    SeparatedList<ParameterSyntax>(parameters)
                )
            )
            .WithBody(
                Block(
                    new StatementSyntax[]
                    {
                        LocalDeclarationStatement(
                            VariableDeclaration(
                                    IdentifierName("var")
                                )
                                .WithVariables(
                                    SingletonSeparatedList(
                                        VariableDeclarator(
                                                Identifier("visitor")
                                            )
                                            .WithInitializer(
                                                EqualsValueClause(
                                                    ObjectCreationExpression(
                                                            ParseTypeName(visitorTypeName)
                                                        )
                                                        .WithArgumentList(
                                                            ArgumentList()
                                                        )
                                                )
                                            )
                                    )
                                )
                        ),
                        ReturnStatement(
                            InvocationExpression(
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("source"),
                                    GenericName(Identifier("Accept"))
                                        .WithTypeArgumentList(
                                            TypeArgumentList(
                                                acceptTypeArguments
                                            )
                                        )
                                ),
                                ArgumentList(
                                    acceptArguments
                                )
                            )
                        )
                    }
                )
            );
    }

    private static bool TryGetTupleParameters(
        ITypeSymbol? argTypeSymbol,
        TypeSyntax argTypeSyntax,
        out IReadOnlyList<TupleArgParameter> tupleParameters)
    {
        var usedNames = new HashSet<string>(StringComparer.Ordinal);

        if (argTypeSymbol is INamedTypeSymbol { IsTupleType: true } tupleTypeSymbol)
        {
            var parameters = new List<TupleArgParameter>(tupleTypeSymbol.TupleElements.Length);
            for (int i = 0; i < tupleTypeSymbol.TupleElements.Length; i++)
            {
                var tupleElement = tupleTypeSymbol.TupleElements[i];
                var fallbackName = $"arg{i + 1}";
                var suggestedName = tupleElement.Name;
                if (string.IsNullOrWhiteSpace(suggestedName) || suggestedName.StartsWith("Item", StringComparison.Ordinal))
                {
                    suggestedName = fallbackName;
                }

                var parameterName = MakeUniqueIdentifier(suggestedName, usedNames, fallbackName);
                parameters.Add(new TupleArgParameter(tupleElement.Type.ToDisplayString(TypeDisplayFormat), parameterName));
            }

            tupleParameters = parameters;
            return true;
        }

        if (argTypeSyntax is TupleTypeSyntax tupleTypeSyntax)
        {
            var parameters = new List<TupleArgParameter>(tupleTypeSyntax.Elements.Count);
            for (int i = 0; i < tupleTypeSyntax.Elements.Count; i++)
            {
                var tupleElement = tupleTypeSyntax.Elements[i];
                var fallbackName = $"arg{i + 1}";
                var suggestedName = string.IsNullOrWhiteSpace(tupleElement.Identifier.Text)
                    ? fallbackName
                    : tupleElement.Identifier.Text;
                var parameterName = MakeUniqueIdentifier(suggestedName, usedNames, fallbackName);
                parameters.Add(new TupleArgParameter(tupleElement.Type.ToString(), parameterName));
            }

            tupleParameters = parameters;
            return true;
        }

        tupleParameters = Array.Empty<TupleArgParameter>();
        return false;
    }

    private static string MakeUniqueIdentifier(string candidate, ISet<string> usedNames, string fallback)
    {
        var normalizedCandidate = candidate.TrimStart('@');
        var normalizedFallback = fallback.TrimStart('@');

        if (!SyntaxFacts.IsValidIdentifier(normalizedCandidate) &&
            SyntaxFacts.GetKeywordKind(normalizedCandidate) == SyntaxKind.None)
        {
            normalizedCandidate = normalizedFallback;
        }

        if (string.IsNullOrWhiteSpace(normalizedCandidate))
        {
            normalizedCandidate = normalizedFallback;
        }

        var uniqueBaseName = normalizedCandidate;
        var suffix = 2;

        while (!usedNames.Add(uniqueBaseName))
        {
            uniqueBaseName = $"{normalizedCandidate}{suffix}";
            suffix++;
        }

        return SyntaxFacts.GetKeywordKind(uniqueBaseName) != SyntaxKind.None
            ? $"@{uniqueBaseName}"
            : uniqueBaseName;
    }

    private static MemberDeclarationSyntax GenerateVoidAccept(bool isPublic, string enumName, IReadOnlyList<string> members)
    {
        var cases = GenerateVoidSwitchesSyntax(enumName, members);

        return MethodDeclaration(
                PredefinedType(
                    Token(SyntaxKind.VoidKeyword)
                ),
                Identifier("Accept")
            )
            .WithModifiers(
                TokenList(
                    Token(isPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword),
                    Token(SyntaxKind.StaticKeyword)
                )
            )
            .WithParameterList(
                ParameterList(
                    SeparatedList<ParameterSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            Parameter(
                                    Identifier("source")
                                )
                                .WithModifiers(
                                    TokenList(
                                        Token(SyntaxKind.ThisKeyword)
                                    )
                                )
                                .WithType(
                                    IdentifierName(enumName)
                                ),
                            Token(SyntaxKind.CommaToken),
                            Parameter(
                                    Identifier("visitor")
                                )
                                .WithType(
                                    IdentifierName($"I{enumName}Visitor")
                                )
                        }
                    )
                )
            )
            .WithBody(
                Block(SingletonList<StatementSyntax>(SwitchStatement(IdentifierName("source")).WithSections(List(cases))))
            );
    }

    private static MemberDeclarationSyntax GenerateVoidStructAccept(bool isPublic, string enumName, IReadOnlyList<string> members)
    {
        var cases = GenerateVoidSwitchesSyntax(enumName, members);

        return MethodDeclaration(
                PredefinedType(
                    Token(SyntaxKind.VoidKeyword)
                ),
                Identifier("Accept")
            )
            .WithModifiers(
                TokenList(
                    Token(isPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword),
                    Token(SyntaxKind.StaticKeyword)
                )
            )
            .WithTypeParameterList(
                TypeParameterList(
                    SingletonSeparatedList(
                        TypeParameter(
                            Identifier("TVisitor")
                        )
                    )
                )
            )
            .WithParameterList(
                ParameterList(
                    SeparatedList<ParameterSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            Parameter(
                                    Identifier("source")
                                )
                                .WithModifiers(
                                    TokenList(
                                        Token(SyntaxKind.ThisKeyword)
                                    )
                                )
                                .WithType(
                                    IdentifierName(enumName)
                                ),
                            Token(SyntaxKind.CommaToken),
                            Parameter(
                                    Identifier("visitor")
                                )
                                .WithModifiers(
                                    TokenList(
                                        Token(SyntaxKind.RefKeyword)
                                    )
                                )
                                .WithType(
                                    IdentifierName("TVisitor")
                                )
                        }
                    )
                )
            )
            .WithConstraintClauses(
                SingletonList(
                    TypeParameterConstraintClause(
                            IdentifierName("TVisitor")
                        )
                        .WithConstraints(
                            SeparatedList<TypeParameterConstraintSyntax>(
                                new SyntaxNodeOrToken[]
                                {
                                    ClassOrStructConstraint(
                                        SyntaxKind.StructConstraint
                                    ),
                                    Token(SyntaxKind.CommaToken),
                                    TypeConstraint(
                                        IdentifierName($"I{enumName}Visitor")
                                    )
                                }
                            )
                        )
                )
            )
            .WithBody(
                Block(SingletonList<StatementSyntax>(SwitchStatement(IdentifierName("source")).WithSections(List(cases))))
            );
    }

    private static MemberDeclarationSyntax GenerateGenericResultAccept(
        bool isPublic,
        bool withArg,
        string enumName,
        IReadOnlyList<string> members)
    {
        var cases = GenerateReturnSwitchesSyntax(withArg, enumName, members);

        var genericParameters = new List<SyntaxNodeOrToken>
        {
            TypeParameter(
                Identifier("T")
            )
        };

        var paramList = new List<SyntaxNodeOrToken>
        {
            Parameter(
                    Identifier("source")
                )
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.ThisKeyword)
                    )
                )
                .WithType(
                    IdentifierName(enumName)
                ),
            Token(SyntaxKind.CommaToken),
            Parameter(
                    Identifier("visitor")
                )
                .WithType(
                    GenericName(Identifier($"I{enumName}Visitor"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                withArg
                                    ? SeparatedList<TypeSyntax>(
                                        new SyntaxNodeOrToken[]
                                        {
                                            IdentifierName("T"),
                                            Token(SyntaxKind.CommaToken),
                                            IdentifierName("TArg")
                                        }
                                    )
                                    : SingletonSeparatedList<TypeSyntax>(
                                        IdentifierName("T")
                                    )
                            )
                        )
                )
        };

        if (withArg)
        {
            genericParameters.Add(Token(SyntaxKind.CommaToken));
            genericParameters.Add(TypeParameter(Identifier("TArg")));

            paramList.Add(Token(SyntaxKind.CommaToken));
            paramList.Add(
                Parameter(
                        Identifier("arg")
                    )
                    .WithType(
                        IdentifierName("TArg")
                    )
            );
        }

        return MethodDeclaration(
                IdentifierName("T"),
                Identifier("Accept")
            )
            .WithModifiers(
                TokenList(
                    Token(isPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword),
                    Token(SyntaxKind.StaticKeyword)
                )
            )
            .WithTypeParameterList(
                TypeParameterList(
                    SeparatedList<TypeParameterSyntax>(
                        genericParameters
                    )
                )
            )
            .WithParameterList(
                ParameterList(
                    SeparatedList<ParameterSyntax>(paramList)
                )
            )
            .WithBody(
                Block(SingletonList<StatementSyntax>(SwitchStatement(IdentifierName("source")).WithSections(List(cases))))
            );
    }

    private static MemberDeclarationSyntax GenerateGenericResultStructAccept(
        bool isPublic,
        bool withArg,
        string enumName,
        IReadOnlyList<string> members)
    {
        var cases = GenerateReturnSwitchesSyntax(withArg, enumName, members);

        var genericParameters = new List<SyntaxNodeOrToken>
        {
            TypeParameter(
                Identifier("T")
            ),
            Token(SyntaxKind.CommaToken),
            TypeParameter(
                Identifier("TVisitor")
            )
        };

        var paramList = new List<SyntaxNodeOrToken>
        {
            Parameter(
                    Identifier("source")
                )
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.ThisKeyword)
                    )
                )
                .WithType(
                    IdentifierName(enumName)
                ),
            Token(SyntaxKind.CommaToken),
            Parameter(
                    Identifier("visitor")
                )
                .WithModifiers(
                    TokenList(
                        Token(SyntaxKind.RefKeyword)
                    )
                )
                .WithType(
                    IdentifierName("TVisitor")
                )
        };

        if (withArg)
        {
            genericParameters.Add(Token(SyntaxKind.CommaToken));
            genericParameters.Add(TypeParameter(Identifier("TArg")));

            paramList.Add(Token(SyntaxKind.CommaToken));
            paramList.Add(
                Parameter(
                        Identifier("arg")
                    )
                    .WithType(
                        IdentifierName("TArg")
                    )
            );
        }

        return MethodDeclaration(
                IdentifierName("T"),
                Identifier("Accept")
            )
            .WithModifiers(
                TokenList(
                    Token(isPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword),
                    Token(SyntaxKind.StaticKeyword)
                )
            )
            .WithTypeParameterList(
                TypeParameterList(
                    SeparatedList<TypeParameterSyntax>(
                        genericParameters
                    )
                )
            )
            .WithParameterList(
                ParameterList(
                    SeparatedList<ParameterSyntax>(
                        paramList
                    )
                )
            )
            .WithConstraintClauses(
                SingletonList(
                    TypeParameterConstraintClause(
                            IdentifierName("TVisitor")
                        )
                        .WithConstraints(
                            SeparatedList<TypeParameterConstraintSyntax>(
                                new SyntaxNodeOrToken[]
                                {
                                    ClassOrStructConstraint(
                                        SyntaxKind.StructConstraint
                                    ),
                                    Token(SyntaxKind.CommaToken),
                                    TypeConstraint(
                                        GenericName(
                                                Identifier($"I{enumName}Visitor")
                                            )
                                            .WithTypeArgumentList(
                                                TypeArgumentList(
                                                    withArg
                                                        ? SeparatedList<TypeSyntax>(
                                                            new SyntaxNodeOrToken[]
                                                            {
                                                                IdentifierName("T"),
                                                                Token(SyntaxKind.CommaToken),
                                                                IdentifierName("TArg")
                                                            }
                                                        )
                                                        : SingletonSeparatedList<TypeSyntax>(
                                                            IdentifierName("T")
                                                        )
                                                )
                                            )
                                    )
                                }
                            )
                        )
                )
            )
            .WithBody(
                Block(SingletonList<StatementSyntax>(SwitchStatement(IdentifierName("source")).WithSections(List(cases))))
            );
    }

    private static MemberDeclarationSyntax GenerateVoidInterface(bool isPublic, string enumName, IReadOnlyList<string> members)
    {
        var methods = new List<MemberDeclarationSyntax>(members.Count);

        foreach (var member in members)
        {
            methods.Add(
                MethodDeclaration(PredefinedType(Token(SyntaxKind.VoidKeyword)), Identifier($"Case{member}"))
                    .WithSemicolonToken(Token(SyntaxKind.SemicolonToken))
            );
        }

        return InterfaceDeclaration($"I{enumName}Visitor")
            .WithModifiers(
                TokenList(
                    Token(isPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword)
                )
            )
            .WithMembers(List(methods));
    }

    static MemberDeclarationSyntax GenerateGenericResultInterface(bool isPublic, string enumName, IReadOnlyList<string> members)
    {
        var methods = new List<MemberDeclarationSyntax>(members.Count);

        foreach (var member in members)
        {
            methods.Add(
                MethodDeclaration(IdentifierName("T"), Identifier($"Case{member}"))
                    .WithSemicolonToken(Token(SyntaxKind.SemicolonToken))
            );
        }

        return InterfaceDeclaration($"I{enumName}Visitor")
            .WithModifiers(
                TokenList(
                    Token(isPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword)
                )
            )
            .WithTypeParameterList(
                TypeParameterList(
                    SingletonSeparatedList(
                        TypeParameter(
                                Identifier("T")
                            )
                            .WithVarianceKeyword(
                                Token(SyntaxKind.OutKeyword)
                            )
                    )
                )
            )
            .WithMembers(List(methods));
    }

    private static MemberDeclarationSyntax GenerateGenericResultInterfaceArgs(
        bool isPublic,
        string enumName,
        IReadOnlyList<string> members)
    {
        var methods = new List<MemberDeclarationSyntax>(members.Count);

        foreach (var member in members)
        {
            methods.Add(
                MethodDeclaration(IdentifierName("T"), Identifier($"Case{member}"))
                    .WithParameterList(
                        ParameterList(
                            SingletonSeparatedList(
                                Parameter(
                                        Identifier("arg")
                                    )
                                    .WithType(
                                        IdentifierName("TArg")
                                    )
                            )
                        )
                    )
                    .WithSemicolonToken(Token(SyntaxKind.SemicolonToken))
            );
        }

        return InterfaceDeclaration($"I{enumName}Visitor")
            .WithModifiers(
                TokenList(
                    Token(isPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword)
                )
            )
            .WithTypeParameterList(
                TypeParameterList(
                    SeparatedList<TypeParameterSyntax>(
                        new SyntaxNodeOrToken[]
                        {
                            TypeParameter(
                                    Identifier("T")
                                )
                                .WithVarianceKeyword(
                                    Token(SyntaxKind.OutKeyword)
                                ),
                            Token(SyntaxKind.CommaToken),
                            TypeParameter(
                                    Identifier("TArg")
                                )
                                .WithVarianceKeyword(
                                    Token(SyntaxKind.InKeyword)
                                )
                        }
                    )
                )
            )
            .WithMembers(List(methods));
    }

    private static List<SwitchSectionSyntax> GenerateVoidSwitchesSyntax(string enumName, IReadOnlyList<string> members)
    {
        var cases = new List<SwitchSectionSyntax>(members.Count + 1);
        foreach (var member in members)
        {
            cases.Add(
                SwitchSection()
                    .WithLabels(
                        SingletonList<SwitchLabelSyntax>(
                            CaseSwitchLabel(
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName(enumName),
                                    IdentifierName(member)
                                )
                            )
                        )
                    )
                    .WithStatements(
                        List(
                            new StatementSyntax[]
                            {
                                ExpressionStatement(
                                    InvocationExpression(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("visitor"),
                                            IdentifierName($"Case{member}")
                                        )
                                    )
                                ),
                                BreakStatement()
                            }
                        )
                    )
            );
        }

        cases.Add(GetDefaultCase());
        return cases;
    }

    private static List<SwitchSectionSyntax> GenerateReturnSwitchesSyntax(
        bool withArg,
        string enumName,
        IReadOnlyList<string> members)
    {
        var cases = new List<SwitchSectionSyntax>(members.Count + 1);
        foreach (var member in members)
        {
            cases.Add(
                SwitchSection()
                    .WithLabels(
                        SingletonList<SwitchLabelSyntax>(
                            CaseSwitchLabel(
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName(enumName),
                                    IdentifierName(member)
                                )
                            )
                        )
                    )
                    .WithStatements(
                        SingletonList<StatementSyntax>(
                            ReturnStatement(
                                withArg
                                    ? InvocationExpression(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("visitor"),
                                            IdentifierName($"Case{member}")
                                        ),
                                        ArgumentList(SingletonSeparatedList(Argument(IdentifierName("arg"))))
                                    )
                                    : InvocationExpression(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("visitor"),
                                            IdentifierName($"Case{member}")
                                        )
                                    )
                            )
                        )
                    )
            );
        }

        cases.Add(GetDefaultCase());
        return cases;
    }

    private static SwitchSectionSyntax GetDefaultCase()
    {
        return SwitchSection()
            .WithLabels(
                SingletonList<SwitchLabelSyntax>(
                    DefaultSwitchLabel()
                )
            )
            .WithStatements(
                SingletonList<StatementSyntax>(
                    ThrowStatement(
                        ObjectCreationExpression(
                                IdentifierName(
                                    "ArgumentOutOfRangeException"
                                )
                            )
                            .WithArgumentList(
                                ArgumentList(
                                    SeparatedList<ArgumentSyntax>(
                                        new SyntaxNodeOrToken[]
                                        {
                                            Argument(
                                                InvocationExpression(
                                                        IdentifierName(
                                                            Identifier(
                                                                TriviaList(),
                                                                SyntaxKind
                                                                    .NameOfKeyword,
                                                                "nameof",
                                                                "nameof",
                                                                TriviaList()
                                                            )
                                                        )
                                                    )
                                                    .WithArgumentList(
                                                        ArgumentList(
                                                            SingletonSeparatedList(
                                                                Argument(
                                                                    IdentifierName(
                                                                        "source"
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                            ),
                                            Token(SyntaxKind.CommaToken),
                                            Argument(
                                                IdentifierName("source")
                                            ),
                                            Token(SyntaxKind.CommaToken),
                                            Argument(
                                                LiteralExpression(
                                                    SyntaxKind
                                                        .NullLiteralExpression
                                                )
                                            )
                                        }
                                    )
                                )
                            )
                    )
                )
            );
    }
}
