using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace EnumVisitorGenerator;

[Generator]
public class EnumVisitorGenerator : IIncrementalGenerator
{
    private record struct SemanticTarget(EnumDeclarationSyntax Enum, AttributeSyntax Attribute, AttributeSyntax? Attribute2);

    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        var enumDeclarations = context.SyntaxProvider.CreateSyntaxProvider(
                predicate: static (syntaxNode, _) => syntaxNode is EnumDeclarationSyntax
                {
                    AttributeLists.Count: > 0, Members.Count: > 0
                },
                transform: GetSemanticTargetForGeneration
            )
            .Where(static x => !ReferenceEquals(x.Enum, null));

        context.RegisterPostInitializationOutput(
            ctx => ctx.AddSource(
                "VisitorGeneratorAttribute.g.cs",
                "namespace EnumVisitorGenerator { [global::System.AttributeUsage(global::System.AttributeTargets.Enum)][global::System.Diagnostics.Conditional(\"ENUM_VISITOR_GENERATOR_USAGES\")] internal class VisitorGeneratorAttribute : global::System.Attribute { } }"
            )
        );

        var enumsWithCompilation = enumDeclarations.Collect();

        context.RegisterSourceOutput(enumsWithCompilation, Execute);
    }

    private static SemanticTarget GetSemanticTargetForGeneration(GeneratorSyntaxContext ctx, CancellationToken ct)
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

                    if (fullName == "EnumVisitorGenerator.VisitorGeneratorAttribute")
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

    private static void Execute(SourceProductionContext ctx, ImmutableArray<SemanticTarget> semanticTargets)
    {
        if (semanticTargets.IsDefaultOrEmpty)
        {
            return;
        }

        var duplicateDetector = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase);

        foreach (var semanticTarget in semanticTargets)
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

            var members = enumDeclarationSyntax.Members.Select(m => m.Identifier.Text).ToList();
            var isPublic = enumDeclarationSyntax.Modifiers.Any(t => t.IsKind(SyntaxKind.PublicKeyword));

            var rootNameSpace = namespaceSyntax.WithMembers(
                List(
                    new[]
                    {
                        ClassDeclaration(helperClassName)
                            .WithModifiers(
                                TokenList(
                                    Token(isPublic ? SyntaxKind.PublicKeyword : SyntaxKind.InternalKeyword),
                                    Token(SyntaxKind.StaticKeyword)
                                )
                            )
                            .WithMembers(
                                List(
                                    new[]
                                    {
                                        GenerateVoidAccept(isPublic, enumName, members),
                                        GenerateVoidStructAccept(isPublic, enumName, members),
                                        GenerateGenericResultAccept(isPublic, false, enumName, members),
                                        GenerateGenericResultStructAccept(isPublic, false, enumName, members),
                                        GenerateGenericResultAccept(isPublic, true, enumName, members),
                                        GenerateGenericResultStructAccept(isPublic, true, enumName, members)
                                    }
                                )
                            ),
                        GenerateVoidInterface(isPublic, enumName, members),
                        GenerateGenericResultInterface(isPublic, enumName, members),
                        GenerateGenericResultInterfaceArgs(isPublic, enumName, members)
                    }
                )
            );

            var compilationUnitSyntax = CompilationUnit()
                .WithUsings(SingletonList(UsingDirective(IdentifierName("System"))))
                .WithMembers(SingletonList<MemberDeclarationSyntax>(rootNameSpace))
                .NormalizeWhitespace();

            ctx.AddSource(hintName, compilationUnitSyntax.ToFullString());
        }
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
