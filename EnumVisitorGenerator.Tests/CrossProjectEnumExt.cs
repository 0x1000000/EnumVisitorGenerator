using EnumVisitorGenerator.IntegrationTests;
using NUnit.Framework;

namespace EnumVisitorGenerator.Tests;

public class CrossProjectEnumExt
{

    [Test]
    public void BasicTest()
    {
        Assert.AreEqual(State.Finish.GetStateName(), "Finish");
    }
}


[VisitorToMethod("GetStateName")]
internal readonly struct MyStruct: IStateVisitor<string>
{
    public string CaseInitial()
    {
        return "Initial";
    }

    public string CaseInProgress()
    {
        return "InProgress";
    }

    public string CaseFinish()
    {
        return "Finish";
    }
}
