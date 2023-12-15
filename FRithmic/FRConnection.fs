namespace FRithmic
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections


module FRConnection =
    type ConnParams = {
        AppName: string;
        AppVersion: string
        AdmCnnctPt: string;
        DomainName: string;
        DmnSrvrAddr: string;
        LicSrvrAddr: string;
        LocBrokAddr: string;
        LoggerAddr: string;
        sCnnctPt: string;
        sMdCnnctPt: string;
        sIhCnnctPt: string;
        sTsCnnctPt: string;
        sPnLCnnctPt: string
        sMdEnvKey: string
        sMdUser: string
        sMdPassword: string
        sIhEnvKey: string
        sIhUser: string
        sIhPassword: string
        sTsEnvKey: string
        sTsUser: string
        sTsPassword: string
        PluginMode: bool;
    }
    
    type ConnType =
        | MarketData
        | HistoricalData
        | TradeServer
        | PnLServer
    
    type LoginParams = {
        Env: string;
        User: string;
        Password: string;
        PluginMode: bool;
    }

    let pluginModeMdCnnctPt = "127.0.0.1:3010"
    let pluginModeIhCnnctPt = "127.0.0.1:3012"
    let pluginModeEnvMap =
        Map.ofList [
            ("RAPI_MD_ENCODING", "4")
            ("RAPI_IH_ENCODING", "4")
        ]

    let getMapOfConnParamToRegex =
        Map.ofList [
            ("AdmCnnctPt", @"\s*REngineParams\.AdmCnnctPt\s*:\s*(\S+)");
            ("DmnSrvrAddr", @"\s*REngineParams\.DmnSrvrAddr\s*:\s*(\S+)");
            ("DomainName", @"\s*REngineParams\.DomainName\s*:\s*(\S+)");
            ("LicSrvrAddr", @"\s*REngineParams\.LicSrvrAddr\s*:\s*(\S+)");
            ("LocBrokAddr", @"\s*REngineParams\.LocBrokAddr\s*:\s*(\S+)");
            ("LoggerAddr", @"\s*REngineParams\.LoggerAddr\s*:\s*(\S+)");
            ("sCnnctPt", @"\s*sCnnctPt\s*:\s*(\S+)");
            ("sMdCnnctPt", @"\s*sMdCnnctPt\s*:\s*(\S+)");
            ("sIhCnnctPt", @"\s*sIhCnnctPt\s*:\s*(\S+)");
            ("sTsCnnctPt", @"\s*sTsCnnctPt\s*:\s*(\S+)");
            ("sPnLCnnctPt", @"\s*sPnLCnnctPt\s*:\s*(\S+)")
        ]

    let getMapOfConnNameToFileContent path =
        let separator = Path.DirectorySeparatorChar.ToString()
        Directory.GetFiles(path) |> Array.toList
        |> List.map (fun fname -> (
            fname
                .Replace(path+separator, "")
                .Replace("_connection_params.txt", "")
                .Replace("_", " "),
            File.ReadAllText fname
            ))
        |> Map.ofList
    
    let getParamsForConnName (name: string) mapOfConnNameToFileContent (mapOfConnNameToRegex: Map<string, string>) =
        let rec matchRegex (fileContent: string) (regex: string) =
            Regex.Match(fileContent, regex).Groups.[1].Value
        match mapOfConnNameToFileContent |> Map.tryFind name with
        | None -> None
        | Some fileContent ->
            let p = {
                AdmCnnctPt   = matchRegex fileContent mapOfConnNameToRegex.["AdmCnnctPt"]
                DomainName   = matchRegex fileContent mapOfConnNameToRegex.["DomainName"]
                DmnSrvrAddr  = matchRegex fileContent mapOfConnNameToRegex.["DmnSrvrAddr"]
                LicSrvrAddr  = matchRegex fileContent mapOfConnNameToRegex.["LicSrvrAddr"]
                LocBrokAddr  = matchRegex fileContent mapOfConnNameToRegex.["LocBrokAddr"]
                LoggerAddr   = matchRegex fileContent mapOfConnNameToRegex.["LoggerAddr"]
                sCnnctPt     = matchRegex fileContent mapOfConnNameToRegex.["sCnnctPt"]
                sMdCnnctPt   = matchRegex fileContent mapOfConnNameToRegex.["sMdCnnctPt"]
                sIhCnnctPt   = matchRegex fileContent mapOfConnNameToRegex.["sIhCnnctPt"]
                sTsCnnctPt   = matchRegex fileContent mapOfConnNameToRegex.["sTsCnnctPt"]
                sPnLCnnctPt  = matchRegex fileContent mapOfConnNameToRegex.["sPnLCnnctPt"]
                sMdEnvKey    = ""
                sMdUser      = ""
                sMdPassword  = ""
                sIhEnvKey    = ""
                sIhUser      = ""
                sIhPassword  = ""
                sTsEnvKey    = ""
                sTsUser      = ""
                sTsPassword  = ""
                PluginMode   = false
                AppName      = ""
                AppVersion   = ""
            }
            Some p

    let getParamsForConnTypes connParams (connCred: Map<ConnType, LoginParams>) =
        let prm1 =
            match connCred.TryGetValue MarketData with
            | true, loginParams ->
                {
                    connParams with
                        sMdEnvKey = loginParams.Env
                        sMdUser = loginParams.User
                        sMdPassword = loginParams.Password
                        sMdCnnctPt = if loginParams.PluginMode then pluginModeMdCnnctPt else connParams.sMdCnnctPt
                        PluginMode = loginParams.PluginMode 
                }
            | false, _ ->
                {
                    connParams with
                        sMdEnvKey = ""
                        sMdUser = ""
                        sMdPassword = ""
                        sMdCnnctPt = "" 
                }
        let prm2 =
            match connCred.TryGetValue HistoricalData with
            | true, loginParams ->
                {
                    prm1 with
                        sIhEnvKey = loginParams.Env
                        sIhUser = loginParams.User
                        sIhPassword = loginParams.Password
                        sIhCnnctPt = if loginParams.PluginMode then pluginModeIhCnnctPt else connParams.sIhCnnctPt
                        PluginMode = loginParams.PluginMode
                }
            | false, _ ->
                {
                    prm1 with
                        sIhEnvKey = ""
                        sIhUser = ""
                        sIhPassword = ""
                        sIhCnnctPt = "" 
                }
        let prm3 =
            match connCred.TryGetValue TradeServer with
            | true, loginParams ->
                {
                    prm2 with
                        sTsEnvKey = loginParams.Env
                        sTsUser = loginParams.User
                        sTsPassword = loginParams.Password
                        sTsCnnctPt = connParams.sTsCnnctPt 
                }
            | false, _ ->
                {
                    prm2 with
                        sTsEnvKey = ""
                        sTsUser = ""
                        sTsPassword = ""
                        sTsCnnctPt = "" 
                }
        let prm4 =
            match connCred.TryGetValue PnLServer with
            | true, loginParams ->
                {
                    prm3 with
                        sTsEnvKey = loginParams.Env
                        sTsUser = loginParams.User
                        sTsPassword = loginParams.Password
                        sTsCnnctPt = connParams.sTsCnnctPt
                        sPnLCnnctPt = connParams.sPnLCnnctPt 
                }
            | false, _ ->
                {
                    prm3 with
                        sPnLCnnctPt = ""
                }
        prm4
