# EnumVisitorGenerator
If you mark an enum with the ```[EnumVisitorGenerator.VisitorGenerator]``` attribute, the source generator will generate several "Visitor" interfaces, where each method will correspond to an item of the target enum. Additionally, an extension class with methods will be generated where the target enum is switched and corresponding visitor interface methods are called.

# Example

## Target Enum
```cs
using EnumVisitorGenerator;

namespace SomeNamespace
{
    [VisitorGenerator]
    public enum Color
    {
        Red,
        Green,
        Blue
    }
}
```

## Generated Code
```cs
using System;

namespace SomeNamespace
{
    public static class ColorEnumExtension
    {
        public static void Accept(this Color source, IColorVisitor visitor)
        {
            switch (source)
            {
                case Color.Red:
                    visitor.CaseRed();
                    break;
                case Color.Green:
                    visitor.CaseGreen();
                    break;
                case Color.Blue:
                    visitor.CaseBlue();
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static void Accept<TVisitor>(this Color source, ref TVisitor visitor)
            where TVisitor : struct, IColorVisitor
        {
            switch (source)
            {
                case Color.Red:
                    visitor.CaseRed();
                    break;
                case Color.Green:
                    visitor.CaseGreen();
                    break;
                case Color.Blue:
                    visitor.CaseBlue();
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static T Accept<T>(this Color source, IColorVisitor<T> visitor)
        {
            switch (source)
            {
                case Color.Red:
                    return visitor.CaseRed();
                case Color.Green:
                    return visitor.CaseGreen();
                case Color.Blue:
                    return visitor.CaseBlue();
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static T Accept<T, TVisitor>(this Color source, ref TVisitor visitor)
            where TVisitor : struct, IColorVisitor<T>
        {
            switch (source)
            {
                case Color.Red:
                    return visitor.CaseRed();
                case Color.Green:
                    return visitor.CaseGreen();
                case Color.Blue:
                    return visitor.CaseBlue();
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static T Accept<T, TArg>(this Color source, IColorVisitor<T, TArg> visitor, TArg arg)
        {
            switch (source)
            {
                case Color.Red:
                    return visitor.CaseRed(arg);
                case Color.Green:
                    return visitor.CaseGreen(arg);
                case Color.Blue:
                    return visitor.CaseBlue(arg);
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }

        public static T Accept<T, TVisitor, TArg>(this Color source, ref TVisitor visitor, TArg arg)
            where TVisitor : struct, IColorVisitor<T, TArg>
        {
            switch (source)
            {
                case Color.Red:
                    return visitor.CaseRed(arg);
                case Color.Green:
                    return visitor.CaseGreen(arg);
                case Color.Blue:
                    return visitor.CaseBlue(arg);
                default:
                    throw new ArgumentOutOfRangeException(nameof(source), source, null);
            }
        }
    }

    public interface IColorVisitor
    {
        void CaseRed();
        void CaseGreen();
        void CaseBlue();
    }

    public interface IColorVisitor<out T>
    {
        T CaseRed();
        T CaseGreen();
        T CaseBlue();
    }

    public interface IColorVisitor<out T, in TArg>
    {
        T CaseRed(TArg arg);
        T CaseGreen(TArg arg);
        T CaseBlue(TArg arg);
    }
}
```

## Usage

```cs
    class ColorTranslation : IColorVisitor<string, bool>
    {
        public string CaseRed(bool eng) => eng ? "Red" : "Rojo";

        public string CaseGreen(bool eng) => eng ? "Green" : "Verde";

        public string CaseBlue(bool eng) => eng ? "Blue" : "Azul";
    }

...

var translation = new ColorTranslation();
Console.WriteLine(Color.Red.Accept(translation, false));
```

## "Allocation Free" Usage
```cs
    struct ColorTranslation : IColorVisitor<string, bool>
    {
        public string CaseRed(bool eng) => eng ? "Red" : "Rojo";

        public string CaseGreen(bool eng) => eng ? "Green" : "Verde";

        public string CaseBlue(bool eng) => eng ? "Blue" : "Azul";
    }
...
var translation = new ColorTranslation();
//boxing does not happen here
Console.WriteLine(Color.Red.Accept<string, ColorTranslation, bool>(ref translation, false));
```