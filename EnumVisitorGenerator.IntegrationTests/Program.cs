using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Jobs;
using BenchmarkDotNet.Running;

namespace EnumVisitorGenerator.IntegrationTests;

[SimpleJob(RuntimeMoniker.HostProcess)]
[MemoryDiagnoser]
public class Program
{
    public static void Main()
    {
        BenchmarkRunner.Run<Program>();
    }

    const int Iterations = 1000;

    [Benchmark]
    public void Class()
    {
        for (int i = 0; i < Iterations; i++)
        {
            var visitor = new VisitorClass();
            Color.Green.Accept(visitor, true);
        }
    }

    [Benchmark]
    public void StructBoxing()
    {
        for (int i = 0; i < Iterations; i++)
        {
            var visitor = new VisitorStruct();
            Color.Green.Accept(visitor, true);
        }
    }

    [Benchmark]
    public void StructNoBoxing()
    {
        for (int i = 0; i < Iterations; i++)
        {
            var visitor = new VisitorStruct();
            Color.Green.Accept<string, VisitorStruct, bool>(ref visitor, true);
        }
    }
}

public struct VisitorClass : IColorVisitor<string, bool>
{
    public string CaseRed(bool eng) => eng ? "Red" : "Rojo";

    public string CaseGreen(bool eng) => eng ? "Green" : "Verde";

    public string CaseBlue(bool eng) => eng ? "Blue" : "Azul";
}

public struct VisitorStruct : IColorVisitor<string, bool>
{
    public string CaseRed(bool eng) => eng ? "Red" : "Rojo";

    public string CaseGreen(bool eng) => eng ? "Green" : "Verde";

    public string CaseBlue(bool eng) => eng ? "Blue" : "Azul";
}