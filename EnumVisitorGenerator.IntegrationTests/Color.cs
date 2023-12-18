using EnumVisitorGenerator;

namespace EnumVisitorGenerator.IntegrationTests
{

    [VisitorGenerator]
    internal enum Color
    {
        Red,
        Green,
        Blue
    }

    [VisitorGenerator]
    public enum State
    {
        Initial,
        InProgress,
        Finish
    }
}

namespace EnumVisitorGenerator.IntegrationTests.SubSpace
{
    [VisitorGenerator]
    public enum State
    {
        Initial2,
        InProgress2,
        Finish2
    }
}