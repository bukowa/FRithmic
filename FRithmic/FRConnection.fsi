namespace FRithmic

module FRConnection =
    val getMapOfConnNameToFileContent: string -> Map<string, string>
    val getMapOfConnParamToRegex: Map<string, string>
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
    
    val getParamsForConnName: string -> Map<string, string> -> Map<string, string> -> ConnParams option
    val getParamsForConnTypes: ConnParams -> Map<ConnType, LoginParams> -> ConnParams
    val pluginModeMdCnnctPt: string
    val pluginModeIhCnnctPt: string
    val pluginModeEnvMap: Map<string, string>
