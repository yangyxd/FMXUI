{$IF RTLVersion>=24}
{$LEGACYIFEND ON}
{$IFEND}

{.$DEFINE USE_DEBUGMODE}   // 是否使用调试模式

{$IFDEF USE_DEBUGMODE}
  {$DEFINE DebugApp}       // 启用调试状态
{$ENDIF}

{.$DEFINE OuputFileLog}    //是否输出日志文件
{.$DEFINE OuputRemoteLog}  //是否输出远程日志消息

{$IFDEF DebugApp}
  {$DEFINE WRITEDEBUG}        //是否在命令行窗口输出调试信息
  {$DEFINE OutputDebug}       //是否在消息框中输出调试信息
{$ENDIF}

{$IFDEF MSWINDOWS}
{.$DEFINE UseUDP}  // 是否使用远程输出日志
{$ENDIF}