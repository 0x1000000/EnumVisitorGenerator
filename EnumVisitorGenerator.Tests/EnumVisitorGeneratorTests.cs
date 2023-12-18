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

            Assert.AreEqual(3, genResult.Count);

            Assert.IsTrue(genResult.ContainsKey("VisitorGeneratorAttribute.g.cs"));
            Assert.IsTrue(genResult.ContainsKey("TestSpace.StateEnumExtension.cs"));
            Assert.IsTrue(genResult.ContainsKey("TestSpace.InternalStateEnumExtension.cs"));

            Assert.AreEqual(
                "namespace EnumVisitorGenerator { [global::System.AttributeUsage(global::System.AttributeTargets.Enum)][global::System.Diagnostics.Conditional(\"ENUM_VISITOR_GENERATOR_USAGES\")] internal class VisitorGeneratorAttribute : global::System.Attribute { } }",
                genResult["VisitorGeneratorAttribute.g.cs"]
            );

            Console.WriteLine(genResult["TestSpace.InternalStateEnumExtension.cs"]);

            const string expected = @"using System;

namespace TestSpace
{
    using EnumVisitorGenerator;

    public static class StateEnumExtension
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

            Assert.AreEqual(expected, genResult["TestSpace.StateEnumExtension.cs"]);

            const string internalExpected = @"using System;

namespace TestSpace
{
    using EnumVisitorGenerator;

    internal static class InternalStateEnumExtension
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
            Assert.AreEqual(internalExpected, genResult["TestSpace.InternalStateEnumExtension.cs"]);
        }
    }

    public static class TestHelper
    {
        public static Dictionary<string, string> Verify(string source, out ImmutableArray<Diagnostic> diagnostics)
        {
            var syntaxTree = CSharpSyntaxTree.ParseText(source);

            // Create a Roslyn compilation for the syntax tree.
            var compilation = CSharpCompilation.Create(
                assemblyName: "Tests",
                syntaxTrees: new[] { syntaxTree },
                new[] { MetadataReference.CreateFromFile(typeof(Attribute).Assembly.Location) }
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
