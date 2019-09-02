@echo off

cd /d %~dp0
set input=%1
set BASE_DIR=%~dp0

set PROCESSES=4
set MAKE_OPTS="{d,'DEBUG_MODE'}"
set MAKE_LOCAL_OPTS="{d,'DEBUG_MODE'},{'LOCAL_MODE',true}"

set NODE_NAME=game_node@192.168.16.117
set COOKIE=123456

if "%input%" == "" goto wait_input
goto fun_run

:wait_input
	set input=
	echo.
	echo ==================
	echo make: 编译服务端源码
	echo imake: 编译单文件
	echo start: 启动运行
	echo clean: 清除beam文件
	echo update: 更新代码
	echo stop: 停止运行
	echo quit: 结束运行
	echo ------------------------------
	set /p input=请输入指令:
	echo ==================
	goto fun_run

:fun_run
	if [%input%]==[make] goto fun_make
	if [%input%]==[imake] goto fun_imake
	if [%input%]==[start] goto fun_start
	if [%input%]==[update] goto fun_update
	if [%input%]==[stop] goto fun_stop
	if [%input%]==[clean] goto fun_clean
	if [%input%]==[quit] goto end
	goto wait_input

:fun_make
	rem 开始编译
	cd %BASE_DIR%
	
	erl -noinput -eval "case make:files([\"src/kernel/makeapp.erl\"],[{outdir, \"ebin\"}]) of error -> halt(1); _ -> halt(0) end"
	erl -noinput -pa ./ebin -eval "case catch makeapp:make(%PROCESSES%,[%MAKE_OPTS%,{outdir, \"ebin\"}]) of up_to_date -> halt(0); error -> halt(1) end."
	goto wait_input

:fun_imake
	rem 编译单文件命令
	cd %BASE_DIR%
	
	gen_localmakefile
	erl -pa ./ebin -noinput -eval "case makeapp:make(%PROCESSES%,[%MAKE_LOCAL_OPTS%,{outdir, \"ebin\"}]) of up_to_date -> halt(0); error -> halt(1) end."
	goto wait_input

:fun_start
	rem 启动应用
	cd %BASE_DIR%
	set strtime=%date:~0,4%%date:~5,2%%date:~8,2%%time:~0,2%%time:~3,2%%time:~6,2%
	set SASL_LOG=./log/log_%strtime%.sasl

	start werl +P 102400 -smp auto -pa "./ebin" ^
		-name %NODE_NAME% -setcookie %COOKIE% ^
		-hidden -kernel net_ticktime 600	^
		-sasl sasl_error_logger {file,\"%SASL_LOG%\"} errlog_type error ^
		-s main start
	goto wait_input

:fun_stop
	start werl -pa "./ebin" ^
		-name stop_%NODE_NAME% ^
		-setcookie %COOKIE% ^
		-eval "catch rpc:call('%NODE_NAME%',main,stop,[]),init:stop()."
	goto wait_input

:fun_clean
	cd %BASE_DIR%
	del /f /s /q *.dump
	del /f /s /q ebin\*.beam
	goto wait_input

:fun_update
	start werl -pa "./ebin" ^
		-name update_%NODE_NAME% ^
		-setcookie %COOKIE% ^
		-eval "catch rpc:call('%NODE_NAME%',makeapp,update,[]),init:stop()."
	goto wait_input

:end