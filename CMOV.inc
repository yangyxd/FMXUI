{说明：
使用 YxdInclude 的应用请复制CMOV到自己的根目录}

{.$DEFINE USE_DEBUGMODE}   // 是否使用调试模式

{$IFDEF USE_DEBUGMODE}
  {$DEFINE DebugApp}       // 启用调试状态
{$ENDIF}

{.$DEFINE OuputFileLog}    //是否输出日志文件
{$DEFINE OuputRemoteLog}  //是否输出远程日志消息

{$IFDEF DebugApp}
  {$DEFINE WRITEDEBUG}        //是否在命令行窗口输出调试信息
  {$DEFINE OutputDebug}       //是否在消息框中输出调试信息
{$ENDIF}

{YxdCommon}
{$DEFINE USE_URLFUNC}         // 是否使用URL函数

{YxdHTTP}
{.$DEFINE USE_CoXml}  // 是否可以使用CoXML方式Get或POST

{YxdPerisistent}
{$DEFINE USE_DBREADWRITE}    // 是否可以从数据库中读出对象
{$DEFINE USE_XMLREADWRITE}   // 是否可以使用XML读写对象
{$DEFINE USE_JSONREADWRITE}  // 是否可以使用JSON读写对象



