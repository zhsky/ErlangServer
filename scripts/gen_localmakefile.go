/*
* @Author: Payton
* @Date:   2019-08-27 11:33:57
* @Last Modified by:   Administrator
* @Last Modified time: 2019-08-27 13:38:44
 */

package main

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

var dot = 0
var file, err = os.OpenFile("LocalMakefile", os.O_WRONLY|os.O_TRUNC|os.O_CREATE, 0644)
var os_type = runtime.GOOS

func handle_file(full_path string, info os.FileInfo, err error) error {
	if info == nil {
		return err
	}

	if strings.Contains(full_path, ".svn") {
		return nil
	}

	if strings.HasSuffix(full_path, ".erl") == false {
		return nil
	}

	if info.IsDir() {
		return nil
	}

	var beam_file = strings.TrimSuffix(info.Name(), ".erl") + ".beam"
	var full_beam_file = "ebin/" + beam_file

	beamfd, err_fd := os.Stat(full_beam_file)
	if err_fd == nil && beamfd.ModTime().Unix() >= info.ModTime().Unix() {
		return nil
	}

	if os_type == "windows" {
		full_path = strings.Replace(full_path, "\\", "/", -1)
	}
	if dot == 1 {
		file.WriteString(",")
	}
	dot = 1
	file.WriteString("\n\t\t'" + full_path + "'")
	return nil
}

func main() {
	if err != nil {
		return
	}
	defer file.Close()

	context := "{\n\t["
	file.WriteString(context)
	filepath.Walk("src", handle_file)
	context = "\n\t],\n\t[\n\t\t{i,\"include/\"},\n\t\tinline,\n\t\twarnings_as_errors\n\t]\n}."
	file.WriteString(context)
}
