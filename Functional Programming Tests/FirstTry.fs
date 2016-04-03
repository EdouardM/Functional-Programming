namespace Functional_Programming.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open FunctionalProgramming

module ``First try of Xunit and FsCheck`` =
    
    [<Fact>]
    let ``test Success``()  =
        Assert.True(true)

    [<Fact>]
    let ``test Failure`` () =
        Assert.True(false)

    [<Property>]
    let ``square is positive`` (x:float) =
        x * x >= 0.

