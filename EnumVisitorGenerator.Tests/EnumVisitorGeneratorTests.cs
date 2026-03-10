using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using NUnit.Framework;

namespace EnumVisitorGenerator.Tests
{
    [TestFixture]
    public class EnumVisitorGeneratorTests
    {
        [Test]
        public void BasicTest()
        {
            // The source code to test
            var source = @"
namespace TestSpace 
{
    using EnumVisitorGenerator;

    [VisitorGenerator]
    public enum State
    {
        Initial2,
        InProgress2,
        Finish2
    }

    public class SubClass
    {
        [VisitorGenerator]
        public enum State
        {
            Initial2,
            InProgress2,
            Finish2
        }
    }

    [VisitorGenerator]
    enum InternalEmptyState {
        
    }

    [VisitorGenerator]
    enum InternalState : int {
        Member1
    }
}
";

            // Pass the source code to our helper and snapshot test the output
            var genResult = TestHelper.Verify(source, out var diagnostics);

            Assert.AreEqual(1, diagnostics.Length);

            Assert.AreEqual(diagnostics[0].Descriptor.Id, "EG0003");

            Assert.AreEqual(4, genResult.Count);

            Assert.IsTrue(genResult.ContainsKey("VisitorGeneratorAttribute.g.cs"));
            Assert.IsTrue(genResult.ContainsKey("VisitorToMethodAttribute.g.cs"));
            Assert.IsTrue(genResult.ContainsKey("TestSpace.StateEnumExtension.cs"));
            Assert.IsTrue(genResult.ContainsKey("TestSpace.InternalStateEnumExtension.cs"));

            Assert.AreEqual(
                "namespace EnumVisitorGenerator { [global::System.AttributeUsage(global::System.AttributeTargets.Enum)][global::System.Diagnostics.Conditional(\"ENUM_VISITOR_GENERATOR_USAGES\")] internal class VisitorGeneratorAttribute : global::System.Attribute { } }",
                genResult["VisitorGeneratorAttribute.g.cs"]
            );
            Assert.AreEqual(
                "namespace EnumVisitorGenerator { [global::System.AttributeUsage(global::System.AttributeTargets.Struct)][global::System.Diagnostics.Conditional(\"ENUM_VISITOR_GENERATOR_USAGES\")] internal class VisitorToMethodAttribute : global::System.Attribute { public VisitorToMethodAttribute(global::System.String methodName) { } } }",
                genResult["VisitorToMethodAttribute.g.cs"]
            );

            const string expected = @"using System;

namespace TestSpace
{
    using EnumVisitorGenerator;

    public static partial class StateEnumExtension
    {
        public static void Accept(this State source, IStateVisitor visitor)
        {
            switch (source)
            {
                case State.Initial2:
                    visitor.CaseInitial2();
                    break;
                case State.InProgress2:
                    visitor.CaseInProgress2();
                    break;
                case State.Finish2:
                    visitor.CaseFinish2();
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static void Accept<TVisitor>(this State source, ref TVisitor visitor)
            where TVisitor : struct, IStateVisitor
        {
            switch (source)
            {
                case State.Initial2:
                    visitor.CaseInitial2();
                    break;
                case State.InProgress2:
                    visitor.CaseInProgress2();
                    break;
                case State.Finish2:
                    visitor.CaseFinish2();
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static T Accept<T>(this State source, IStateVisitor<T> visitor)
        {
            switch (source)
            {
                case State.Initial2:
                    return visitor.CaseInitial2();
                case State.InProgress2:
                    return visitor.CaseInProgress2();
                case State.Finish2:
                    return visitor.CaseFinish2();
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static T Accept<T, TVisitor>(this State source, ref TVisitor visitor)
            where TVisitor : struct, IStateVisitor<T>
        {
            switch (source)
            {
                case State.Initial2:
                    return visitor.CaseInitial2();
                case State.InProgress2:
                    return visitor.CaseInProgress2();
                case State.Finish2:
                    return visitor.CaseFinish2();
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static T Accept<T, TArg>(this State source, IStateVisitor<T, TArg> visitor, TArg arg)
        {
            switch (source)
            {
                case State.Initial2:
                    return visitor.CaseInitial2(arg);
                case State.InProgress2:
                    return visitor.CaseInProgress2(arg);
                case State.Finish2:
                    return visitor.CaseFinish2(arg);
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static T Accept<T, TVisitor, TArg>(this State source, ref TVisitor visitor, TArg arg)
            where TVisitor : struct, IStateVisitor<T, TArg>
        {
            switch (source)
            {
                case State.Initial2:
                    return visitor.CaseInitial2(arg);
                case State.InProgress2:
                    return visitor.CaseInProgress2(arg);
                case State.Finish2:
                    return visitor.CaseFinish2(arg);
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }
    }

    public interface IStateVisitor
    {
        void CaseInitial2();
        void CaseInProgress2();
        void CaseFinish2();
    }

    public interface IStateVisitor<out T>
    {
        T CaseInitial2();
        T CaseInProgress2();
        T CaseFinish2();
    }

    public interface IStateVisitor<out T, in TArg>
    {
        T CaseInitial2(TArg arg);
        T CaseInProgress2(TArg arg);
        T CaseFinish2(TArg arg);
    }
}";

            Assert.AreEqual(TestHelper.NormalizeNewLines(expected), TestHelper.NormalizeNewLines(genResult["TestSpace.StateEnumExtension.cs"]));

            const string internalExpected = @"using System;

namespace TestSpace
{
    using EnumVisitorGenerator;

    internal static partial class InternalStateEnumExtension
    {
        internal static void Accept(this InternalState source, IInternalStateVisitor visitor)
        {
            switch (source)
            {
                case InternalState.Member1:
                    visitor.CaseMember1();
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        internal static void Accept<TVisitor>(this InternalState source, ref TVisitor visitor)
            where TVisitor : struct, IInternalStateVisitor
        {
            switch (source)
            {
                case InternalState.Member1:
                    visitor.CaseMember1();
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        internal static T Accept<T>(this InternalState source, IInternalStateVisitor<T> visitor)
        {
            switch (source)
            {
                case InternalState.Member1:
                    return visitor.CaseMember1();
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        internal static T Accept<T, TVisitor>(this InternalState source, ref TVisitor visitor)
            where TVisitor : struct, IInternalStateVisitor<T>
        {
            switch (source)
            {
                case InternalState.Member1:
                    return visitor.CaseMember1();
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        internal static T Accept<T, TArg>(this InternalState source, IInternalStateVisitor<T, TArg> visitor, TArg arg)
        {
            switch (source)
            {
                case InternalState.Member1:
                    return visitor.CaseMember1(arg);
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        internal static T Accept<T, TVisitor, TArg>(this InternalState source, ref TVisitor visitor, TArg arg)
            where TVisitor : struct, IInternalStateVisitor<T, TArg>
        {
            switch (source)
            {
                case InternalState.Member1:
                    return visitor.CaseMember1(arg);
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }
    }

    internal interface IInternalStateVisitor
    {
        void CaseMember1();
    }

    internal interface IInternalStateVisitor<out T>
    {
        T CaseMember1();
    }

    internal interface IInternalStateVisitor<out T, in TArg>
    {
        T CaseMember1(TArg arg);
    }
}";
            Assert.AreEqual(TestHelper.NormalizeNewLines(internalExpected), TestHelper.NormalizeNewLines(genResult["TestSpace.InternalStateEnumExtension.cs"]));
        }

        [Test]
        public void VisitorToMethodGeneratesWrapperAndTupleDestructuring()
        {
            var source = @"
namespace TestSpace
{
    using EnumVisitorGenerator;

    [VisitorGenerator]
    public enum Color
    {
        Red,
        Green,
        Blue
    }

    [VisitorToMethod(""GetColor"")]
    public struct VisitorStruct : IColorVisitor<string, (bool eng, int repeat)>
    {
        public string CaseRed((bool eng, int repeat) arg) => arg.eng ? ""Red"" : ""Rojo"";
        public string CaseGreen((bool eng, int repeat) arg) => arg.eng ? ""Green"" : ""Verde"";
        public string CaseBlue((bool eng, int repeat) arg) => arg.eng ? ""Blue"" : ""Azul"";
    }
}";

            var genResult = TestHelper.Verify(source, out var diagnostics);

            Assert.IsEmpty(diagnostics);
            Assert.IsTrue(genResult.ContainsKey("TestSpace.ColorEnumExtension.cs"));

            var generated = genResult["TestSpace.ColorEnumExtension.cs"];
            StringAssert.Contains("public static string GetColor(this Color source, bool eng, int repeat)", generated);
            StringAssert.Contains("var visitor = new global::TestSpace.VisitorStruct();", generated);
            StringAssert.Contains("return source.Accept<string, global::TestSpace.VisitorStruct", generated);
            StringAssert.Contains(">(ref visitor, (eng, repeat));", generated);
        }

        [Test]
        public void VisitorToMethod_GeneratesWrapperWithoutArgument()
        {
            var source = @"
namespace TestSpace
{
    using EnumVisitorGenerator;

    [VisitorGenerator]
    public enum Color
    {
        Red,
        Green
    }

    [VisitorToMethod(""GetColor"")]
    public struct VisitorStruct : IColorVisitor<string>
    {
        public string CaseRed() => ""R"";
        public string CaseGreen() => ""G"";
    }
}";

            var genResult = TestHelper.Verify(source, out var diagnostics);

            Assert.IsEmpty(diagnostics);
            var generated = genResult["TestSpace.ColorEnumExtension.cs"];
            StringAssert.Contains("public static string GetColor(this Color source)", generated);
            StringAssert.Contains("return source.Accept<string, global::TestSpace.VisitorStruct>(ref visitor);", generated);
        }


        [Test]
        public void VisitorToMethod_GeneratesWrapperForReferencedEnum()
        {
            var source = @"
namespace TestSpace
{
    using EnumVisitorGenerator;
    using EnumVisitorGenerator.IntegrationTests;

    [VisitorToMethod(""GetStateName"")]
    public struct ExternalVisitor : IStateVisitor<string, bool>
    {
        public string CaseInitial(bool eng) => eng ? ""Initial"" : ""Inicio"";

        public string CaseInProgress(bool eng) => eng ? ""InProgress"" : ""EnProgreso"";

        public string CaseFinish(bool eng) => eng ? ""Finish"" : ""Fin"";
    }
}";

            var genResult = TestHelper.Verify(
                source,
                out var diagnostics,
                TestHelper.GetIntegrationTestsReference()
            );

            Assert.IsEmpty(diagnostics);
            Assert.IsTrue(genResult.ContainsKey("EnumVisitorGenerator.IntegrationTests.StateEnumExtension.VisitorToMethod.cs"));

            var generated = genResult["EnumVisitorGenerator.IntegrationTests.StateEnumExtension.VisitorToMethod.cs"];
            StringAssert.Contains("namespace EnumVisitorGenerator.IntegrationTests", generated);
            StringAssert.Contains("public static partial class StateEnumExtension", generated);
            StringAssert.Contains("public static string GetStateName(this State source, bool arg)", generated);
            StringAssert.Contains("return source.Accept<string, global::TestSpace.ExternalVisitor, bool>(ref visitor, arg);", generated);
        }

        [Test]
        public void VisitorToMethod_AllowsOverloadsWithDifferentArguments()
        {
            var source = @"
namespace TestSpace
{
    using EnumVisitorGenerator;

    [VisitorGenerator]
    public enum Color
    {
        Red,
        Green
    }

    [VisitorToMethod(""GetColor"")]
    public struct VisitorNoArg : IColorVisitor<string>
    {
        public string CaseRed() => ""R"";
        public string CaseGreen() => ""G"";
    }

    [VisitorToMethod(""GetColor"")]
    public struct VisitorBool : IColorVisitor<string, bool>
    {
        public string CaseRed(bool arg) => ""R"";
        public string CaseGreen(bool arg) => ""G"";
    }

    [VisitorToMethod(""GetColor"")]
    public struct VisitorInt : IColorVisitor<string, int>
    {
        public string CaseRed(int arg) => ""R"";
        public string CaseGreen(int arg) => ""G"";
    }
}";

            var genResult = TestHelper.Verify(source, out var diagnostics);

            Assert.IsEmpty(diagnostics);
            var generated = genResult["TestSpace.ColorEnumExtension.cs"];
            StringAssert.Contains("public static string GetColor(this Color source)", generated);
            StringAssert.Contains("public static string GetColor(this Color source, bool arg)", generated);
            StringAssert.Contains("public static string GetColor(this Color source, int arg)", generated);
        }

        [Test]
        public void VisitorToMethod_ReportsCollisionForSameSignature()
        {
            var source = @"
namespace TestSpace
{
    using EnumVisitorGenerator;

    [VisitorGenerator]
    public enum Color
    {
        Red,
        Green
    }

    [VisitorToMethod(""GetColor"")]
    public struct VisitorBool1 : IColorVisitor<string, bool>
    {
        public string CaseRed(bool arg) => ""R"";
        public string CaseGreen(bool arg) => ""G"";
    }

    [VisitorToMethod(""GetColor"")]
    public struct VisitorBool2 : IColorVisitor<string, bool>
    {
        public string CaseRed(bool arg) => ""R"";
        public string CaseGreen(bool arg) => ""G"";
    }
}";

            TestHelper.Verify(source, out var diagnostics);

            Assert.AreEqual(1, diagnostics.Length);
            Assert.AreEqual("EG0008", diagnostics[0].Descriptor.Id);
        }

        [Test]
        public void VisitorToMethod_ReportsWhenNoGeneratedVisitorInterfaceImplemented()
        {
            var source = @"
namespace TestSpace
{
    using EnumVisitorGenerator;

    [VisitorGenerator]
    public enum Color
    {
        Red,
        Green
    }

    [VisitorToMethod(""GetColor"")]
    public struct InvalidVisitor
    {
    }
}";

            TestHelper.Verify(source, out var diagnostics);

            Assert.AreEqual(1, diagnostics.Length);
            Assert.AreEqual("EG0005", diagnostics[0].Descriptor.Id);
        }

        [Test]
        public void VisitorToMethod_ReportsWhenMultipleGeneratedVisitorInterfacesImplemented()
        {
            var source = @"
namespace TestSpace
{
    using EnumVisitorGenerator;

    [VisitorGenerator]
    public enum Color
    {
        Red,
        Green
    }

    [VisitorToMethod(""GetColor"")]
    public struct InvalidVisitor : IColorVisitor<string, bool>, IColorVisitor<string, int>
    {
        public string CaseRed(bool arg) => ""R"";
        public string CaseGreen(bool arg) => ""G"";
        public string CaseRed(int arg) => ""R"";
        public string CaseGreen(int arg) => ""G"";
    }
}";

            TestHelper.Verify(source, out var diagnostics);

            Assert.AreEqual(1, diagnostics.Length);
            Assert.AreEqual("EG0009", diagnostics[0].Descriptor.Id);
        }
    }

    public static class TestHelper
    {
        public static string NormalizeNewLines(string input) => input.Replace("\r\n", "\n").Replace("\r", "\n");

        public static MetadataReference GetIntegrationTestsReference()
        {
            var current = new DirectoryInfo(AppContext.BaseDirectory);
            while (current != null && !File.Exists(Path.Combine(current.FullName, "EnumVisitorGenerator.sln")))
            {
                current = current.Parent;
            }

            if (current == null)
            {
                throw new DirectoryNotFoundException("Could not locate solution root for integration test reference.");
            }

            var assemblyPath = Path.Combine(
                current.FullName,
                "EnumVisitorGenerator.IntegrationTests",
                "bin",
                "Debug",
                "net8.0",
                "EnumVisitorGenerator.IntegrationTests.dll"
            );

            if (!File.Exists(assemblyPath))
            {
                throw new FileNotFoundException("Integration test assembly was not found.", assemblyPath);
            }

            return MetadataReference.CreateFromFile(assemblyPath);
        }

        public static Dictionary<string, string> Verify(
            string source,
            out ImmutableArray<Diagnostic> diagnostics,
            params MetadataReference[] additionalReferences)
        {
            var syntaxTree = CSharpSyntaxTree.ParseText(source);
            var references = AppDomain.CurrentDomain.GetAssemblies()
                .Where(a => !a.IsDynamic && !string.IsNullOrWhiteSpace(a.Location))
                .Select(a => MetadataReference.CreateFromFile(a.Location))
                .Concat(additionalReferences)
                .GroupBy(r => r.Display, StringComparer.OrdinalIgnoreCase)
                .Select(g => g.First())
                .ToArray();

            // Create a Roslyn compilation for the syntax tree.
            var compilation = CSharpCompilation.Create(
                assemblyName: "Tests",
                syntaxTrees: new[] { syntaxTree },
                references
            );

            // Create an instance of our EnumGenerator incremental source generator
            var generator = new EnumVisitorGenerator();

            // The GeneratorDriver is used to run our generator against a compilation
            GeneratorDriver driver = CSharpGeneratorDriver.Create(generator);

            // Run the source generator!
            driver = driver.RunGenerators(compilation);

            var runResult = driver.GetRunResult();

            diagnostics = runResult.Diagnostics;

            return runResult.GeneratedTrees.ToDictionary(t => Path.GetFileName(t.FilePath), t => t.GetText().ToString());
        }
    }
}
