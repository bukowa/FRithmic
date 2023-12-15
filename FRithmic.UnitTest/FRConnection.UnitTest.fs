module FRithmic.UnitTest

open System.IO
open System.Text.RegularExpressions
open NUnit.Framework
open FRithmic.FRConnection;


let dsep = string Path.DirectorySeparatorChar

let connFolderPath = __SOURCE_DIRECTORY__ + dsep + "connections"

[<TestFixture>]
type LibraryTests () =
    
    [<Test>]
    member this.getMapOfConnNameToFileContent() =
        let ccmap = getMapOfConnNameToFileContent connFolderPath
        Assert.That(ccmap.Count = 152, Is.True, ccmap.Count.ToString())
        Assert.That(ccmap.Keys |> Seq.head, Is.EqualTo("Apex Chicago Area"))
        Assert.That(ccmap.Values |> Seq.forall (fun x -> x.Length > 100), Is.True)
        Assert.That(ccmap.Values |> Seq.forall (fun x -> x.Contains "REngineParams.DmnSrvrAddr"), Is.True)
    
    [<Test>]
    member this.getMapOfConnPropertyToRegex() =
        let cptr = getMapOfConnParamToRegex
        let ccmap = getMapOfConnNameToFileContent connFolderPath
        let canFind = ccmap.Values |> Seq.forall (
            fun x -> cptr.Values |> Seq.forall (
                fun y -> Regex.Matches(x, y).[0] |> fun z -> z.Length > 10))
        Assert.That(canFind, Is.True)
    
    [<Test>]
    member this.getParamsForConnName() =
        let ccmap = getMapOfConnNameToFileContent connFolderPath
        let cptr = getMapOfConnParamToRegex
        let connName = "Apex Chicago Area"
        let prms = getParamsForConnName connName ccmap cptr
        Assert.That(prms.Value.DomainName, Is.EqualTo("rithmic_prod_01_dmz_domain"))
        Assert.That(prms.Value.DmnSrvrAddr, Is.EqualTo("ritpz01001.01.rithmic.com:65000~ritpz01000.01.rithmic.com:65000~ritpz01001.01.rithmic.net:65000~ritpz01000.01.rithmic.net:65000~ritpz01001.01.theomne.net:65000~ritpz01000.01.theomne.net:65000~ritpz01001.01.theomne.com:65000~ritpz01000.01.theomne.com:65000"))
        Assert.That(prms.Value.LicSrvrAddr, Is.EqualTo("ritpz01000.01.rithmic.com:56000~ritpz01001.01.rithmic.com:56000~ritpz01000.01.rithmic.net:56000~ritpz01001.01.rithmic.net:56000~ritpz01000.01.theomne.net:56000~ritpz01001.01.theomne.net:56000~ritpz01000.01.theomne.com:56000~ritpz01001.01.theomne.com:56000"))
        Assert.That(prms.Value.LocBrokAddr, Is.EqualTo("ritpz01000.01.rithmic.com:64100"))
        Assert.That(prms.Value.LoggerAddr, Is.EqualTo("ritpz01000.01.rithmic.com:45454~ritpz01000.01.rithmic.net:45454~ritpz01000.01.theomne.net:45454~ritpz01000.01.theomne.com:45454"))
    
    [<Test>]
    member this.getParamsForConnTypes() =
        let ccmap = getMapOfConnNameToFileContent connFolderPath
        let cptr = getMapOfConnParamToRegex
        let connName = "Apex Chicago Area"
        let connParams = getParamsForConnName connName ccmap cptr
        match connParams with
        | None -> Assert.Fail("connParams is None")
        | Some connParams ->
        
        // market data
        let loginMarket = { User = "user"; Password = "password"; Env="system"; PluginMode = false }
        let connCredMarket = Map.ofList [(ConnType.MarketData, loginMarket)]
        let connParamsMarket = getParamsForConnTypes connParams connCredMarket
        Assert.That(connParamsMarket.sMdUser, Is.EqualTo("user"))
        Assert.That(connParamsMarket.sMdPassword, Is.EqualTo("password"))
        Assert.That(connParamsMarket.sMdEnvKey, Is.EqualTo("system"))
        Assert.That(connParamsMarket.sMdCnnctPt, Is.EqualTo(connParams.sMdCnnctPt))
        Assert.That(connParamsMarket.PluginMode, Is.False)
        
        Assert.That(connParamsMarket.sTsCnnctPt, Is.Empty)
        Assert.That(connParamsMarket.sPnLCnnctPt, Is.Empty)
        Assert.That(connParamsMarket.sIhCnnctPt, Is.Empty)
        
        // market data plugin
        let loginMarketPlugin = { User = "user2"; Password = "password2"; Env="system2"; PluginMode = true }
        let connCredMarketPlugin = Map.ofList [(ConnType.MarketData, loginMarketPlugin)]
        let connParamsMarketPlugin = getParamsForConnTypes connParams connCredMarketPlugin
        
        Assert.That(connParamsMarketPlugin.sMdUser, Is.EqualTo("user2"))
        Assert.That(connParamsMarketPlugin.sMdPassword, Is.EqualTo("password2"))
        Assert.That(connParamsMarketPlugin.sMdEnvKey, Is.EqualTo("system2"))
        Assert.That(connParamsMarketPlugin.sMdCnnctPt, Is.EqualTo(pluginModeMdCnnctPt))
        Assert.That(connParamsMarketPlugin.PluginMode, Is.True)
        
        Assert.That(connParamsMarket.sTsCnnctPt, Is.Empty)
        Assert.That(connParamsMarket.sPnLCnnctPt, Is.Empty)
        Assert.That(connParamsMarket.sIhCnnctPt, Is.Empty)
        
        // historical data
        let connCredHist = Map.ofList [(ConnType.HistoricalData, loginMarket)]
        let connParamsHist = getParamsForConnTypes connParams connCredHist
        Assert.That(connParamsHist.sIhUser, Is.EqualTo("user"))
        Assert.That(connParamsHist.sIhPassword, Is.EqualTo("password"))
        Assert.That(connParamsHist.sIhEnvKey, Is.EqualTo("system"))
        Assert.That(connParamsHist.sIhCnnctPt, Is.EqualTo(connParams.sIhCnnctPt))
        Assert.That(connParamsHist.PluginMode, Is.False)
        
        Assert.That(connParamsHist.sTsCnnctPt, Is.Empty)
        Assert.That(connParamsHist.sPnLCnnctPt, Is.Empty)
        Assert.That(connParamsHist.sMdCnnctPt, Is.Empty)
        
        // historical data plugin
        let connCredHistPlugin = Map.ofList [(ConnType.HistoricalData, loginMarketPlugin)]
        let connParamsHistPlugin = getParamsForConnTypes connParams connCredHistPlugin
        Assert.That(connParamsHistPlugin.sIhUser, Is.EqualTo("user2"))
        Assert.That(connParamsHistPlugin.sIhPassword, Is.EqualTo("password2"))
        Assert.That(connParamsHistPlugin.sIhEnvKey, Is.EqualTo("system2"))
        Assert.That(connParamsHistPlugin.sIhCnnctPt, Is.EqualTo(pluginModeIhCnnctPt))
        Assert.That(connParamsHistPlugin.PluginMode, Is.True)
        
        Assert.That(connParamsHist.sTsCnnctPt, Is.Empty)
        Assert.That(connParamsHist.sPnLCnnctPt, Is.Empty)
        Assert.That(connParamsHist.sMdCnnctPt, Is.Empty)
        
        // market + historical data
        let connCredMarketHist = Map.ofList [(ConnType.MarketData, loginMarket); (ConnType.HistoricalData, loginMarket)]
        let connParamsMarketHist = getParamsForConnTypes connParams connCredMarketHist
        Assert.That(connParamsMarketHist.sMdUser, Is.EqualTo("user"))
        Assert.That(connParamsMarketHist.sMdPassword, Is.EqualTo("password"))
        Assert.That(connParamsMarketHist.sMdEnvKey, Is.EqualTo("system"))
        Assert.That(connParamsMarketHist.sMdCnnctPt, Is.EqualTo(connParams.sMdCnnctPt))
        Assert.That(connParamsMarketHist.PluginMode, Is.False)
        Assert.That(connParamsMarketHist.sIhUser, Is.EqualTo("user"))
        Assert.That(connParamsMarketHist.sIhPassword, Is.EqualTo("password"))
        Assert.That(connParamsMarketHist.sIhEnvKey, Is.EqualTo("system"))
        Assert.That(connParamsMarketHist.sIhCnnctPt, Is.EqualTo(connParams.sIhCnnctPt))
        Assert.That(connParamsMarketHist.PluginMode, Is.False)
        
        Assert.That(connParamsMarketHist.sTsCnnctPt, Is.Empty)
        Assert.That(connParamsMarketHist.sPnLCnnctPt, Is.Empty)
        
        
        // market + historical data plugin
        let connCredMarketHistPlugin = Map.ofList [(ConnType.MarketData, loginMarketPlugin); (ConnType.HistoricalData, loginMarketPlugin)]
        let connParamsMarketHistPlugin = getParamsForConnTypes connParams connCredMarketHistPlugin
        Assert.That(connParamsMarketHistPlugin.sMdUser, Is.EqualTo("user2"))
        Assert.That(connParamsMarketHistPlugin.sMdPassword, Is.EqualTo("password2"))
        Assert.That(connParamsMarketHistPlugin.sMdEnvKey, Is.EqualTo("system2"))
        Assert.That(connParamsMarketHistPlugin.sMdCnnctPt, Is.EqualTo(pluginModeMdCnnctPt))
        Assert.That(connParamsMarketHistPlugin.PluginMode, Is.True)
        Assert.That(connParamsMarketHistPlugin.sIhUser, Is.EqualTo("user2"))
        Assert.That(connParamsMarketHistPlugin.sIhPassword, Is.EqualTo("password2"))
        Assert.That(connParamsMarketHistPlugin.sIhEnvKey, Is.EqualTo("system2"))
        Assert.That(connParamsMarketHistPlugin.sIhCnnctPt, Is.EqualTo(pluginModeIhCnnctPt))
        Assert.That(connParamsMarketHistPlugin.PluginMode, Is.True)
        
        Assert.That(connParamsMarketHistPlugin.sTsCnnctPt, Is.Empty)
        Assert.That(connParamsMarketHistPlugin.sPnLCnnctPt, Is.Empty)
        
        // trade server
        let connCredOrder = Map.ofList [(ConnType.TradeServer, loginMarket)]
        let connParamsTradeServer = getParamsForConnTypes connParams connCredOrder
        Assert.That(connParamsTradeServer.sTsPassword, Is.EqualTo("password"))
        Assert.That(connParamsTradeServer.sTsUser, Is.EqualTo("user"))
        Assert.That(connParamsTradeServer.sTsEnvKey, Is.EqualTo("system"))
        Assert.That(connParamsTradeServer.sTsCnnctPt, Is.EqualTo(connParams.sTsCnnctPt))
        Assert.That(connParamsTradeServer.PluginMode, Is.False)
        
        Assert.That(connParamsTradeServer.sPnLCnnctPt, Is.Empty)
        Assert.That(connParamsTradeServer.sMdCnnctPt, Is.Empty)
        Assert.That(connParamsTradeServer.sIhCnnctPt, Is.Empty)
        
        // pnl server
        let connCredPnl = Map.ofList [(ConnType.PnLServer, loginMarket)]
        let connParamsPnl = getParamsForConnTypes connParams connCredPnl
        Assert.That(connParamsPnl.sTsPassword, Is.EqualTo("password"))
        Assert.That(connParamsPnl.sTsUser, Is.EqualTo("user"))
        Assert.That(connParamsPnl.sTsEnvKey, Is.EqualTo("system"))
        Assert.That(connParamsPnl.sTsCnnctPt, Is.EqualTo(connParams.sTsCnnctPt))
        Assert.That(connParamsPnl.sPnLCnnctPt, Is.EqualTo(connParams.sPnLCnnctPt))
        Assert.That(connParamsPnl.PluginMode, Is.False)
        
        Assert.That(connParamsPnl.sMdCnnctPt, Is.Empty)
        Assert.That(connParamsPnl.sIhCnnctPt, Is.Empty)
        
        
        // all
        let connCredAll = Map.ofList [(ConnType.MarketData, loginMarket); (ConnType.HistoricalData, loginMarket); (ConnType.TradeServer, loginMarket); (ConnType.PnLServer, loginMarket)]
        let connParamsAll = getParamsForConnTypes connParams connCredAll
        Assert.That(connParamsAll.sMdUser, Is.EqualTo("user"))
        Assert.That(connParamsAll.sMdPassword, Is.EqualTo("password"))
        Assert.That(connParamsAll.sMdEnvKey, Is.EqualTo("system"))
        Assert.That(connParamsAll.sMdCnnctPt, Is.EqualTo(connParams.sMdCnnctPt))
        Assert.That(connParamsAll.PluginMode, Is.False)
        Assert.That(connParamsAll.sIhUser, Is.EqualTo("user"))
        Assert.That(connParamsAll.sIhPassword, Is.EqualTo("password"))
        Assert.That(connParamsAll.sIhEnvKey, Is.EqualTo("system"))
        Assert.That(connParamsAll.sIhCnnctPt, Is.EqualTo(connParams.sIhCnnctPt))
        Assert.That(connParamsAll.PluginMode, Is.False)
        Assert.That(connParamsAll.sTsPassword, Is.EqualTo("password"))
        Assert.That(connParamsAll.sTsUser, Is.EqualTo("user"))
        Assert.That(connParamsAll.sTsEnvKey, Is.EqualTo("system"))
        Assert.That(connParamsAll.sTsCnnctPt, Is.EqualTo(connParams.sTsCnnctPt))
        Assert.That(connParamsAll.PluginMode, Is.False)
        Assert.That(connParamsAll.sPnLCnnctPt, Is.EqualTo(connParams.sPnLCnnctPt))
