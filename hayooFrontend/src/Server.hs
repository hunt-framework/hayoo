module Main where

import Hayoo.Server (start)
import Hayoo.Common
import System.Console.CmdArgs (cmdArgs, (&=), explicit, name, help, summary)

hayooConfiguration :: HayooConfiguration
hayooConfiguration = HayooConfiguration {
        hayooHost = "*"  &= explicit &= name "hayoo-host" 
            &= help "Which host to bind: * means any, *4 means any IPv4, *6 means any IPv6, you can also specify a specific host.",
        hayooPort = (8080::Int)  &= explicit &= name "hayoo-port" 
            &= help "Listen on this Port",
        huntUrl = "http://localhost:3000/" &= explicit &= name "hunt-url"
            &= help "Url of the Hunt Search Engine (default: http://localhost:3000/)"
    } &= summary "Hayoo Search Frontend"

main :: IO ()  
main = do
    config <- cmdArgs hayooConfiguration
    start config