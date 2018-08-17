{-# LANGUAGE TypeOperators #-}
module SystemHook where

import Data.ByteString.Lazy.Char8 as CH

import Query
import qualified SystemHook.User as U
import qualified SystemHook.Push as P
import qualified SystemHook.GroupM as GM
import qualified SystemHook.GroupCR as GCR
import qualified SystemHook.Key as K
import qualified SystemHook.ProjectCDU as PCDU
import qualified SystemHook.ProjectRT as PRT
import qualified SystemHook.Team as T
import qualified SystemHook.ProjectCreate as PC

sysHookDecoder :: ByteString -> Either String (Either ErrorResp (U.SResp `Sum` P.SResp
                                                                         `Sum` P.SResp
                                                                         `Sum` GM.SResp
                                                                         `Sum` GCR.SResp
                                                                         `Sum` K.SResp
                                                                         `Sum` PCDU.SResp
                                                                         `Sum` PRT.SResp
                                                                         `Sum` T.SResp
                                                                         `Sum` PC.SResp))
sysHookDecoder b = (U.decodeRsp b) <++> (P.pushDecodeRsp b)
                                   <++> (P.tagPushDecodeRsp b)
                                   <++> (GM.decodeRsp b)
                                   <++> (GCR.decodeRsp b)
                                   <++> (K.decodeRsp b)
                                   <++> (PCDU.decodeRsp b)
                                   <++> (PRT.decodeRsp b)
                                   <++> (T.decodeRsp b)
                                   <++> (PC.decodeRsp b)

