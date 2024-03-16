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
        var visitor = new VisitorClass();
        for (int i = 0; i < Iterations; i++)
        {
            Color.Green.Accept(visitor, true);
        }
    }

    [Benchmark]
    public void StructBoxing()
    {
        var visitor = new VisitorStruct();
        for (int i = 0; i < Iterations; i++)
        {
            Color.Green.Accept(visitor, true);
        }
    }

    [Benchmark]
    public void StructNoBoxing()
    {
        var visitor = new VisitorStruct();
        for (int i = 0; i < Iterations; i++)
        {
            Color.Green.Accept<string, VisitorStruct, bool>(ref visitor, true);
        }
    }
}

public class VisitorClass : IColorVisitor<string, bool>
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