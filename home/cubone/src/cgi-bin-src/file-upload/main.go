package main

import (
	"fmt"
	"io"
	"net/http/cgi"
	"os"
	"strings"
	"time"
)

func main() {
	// Read destination directory from environment variable
	destDir := os.Getenv("UPLOAD_DEST_DIR")

	if destDir == "" {
		internalServerError("UPLOAD_DEST_DIR environment variable not set")
		return
	}

	// Read the CGI request
	r, err := cgi.Request()
	if err != nil {
		internalServerError(err.Error())
		return
	}

	// If the method is GET then show the form
	if r.Method == "GET" {
		getForm()
		return
	}

	// Parse the multipart form
	if err = r.ParseMultipartForm(65536); err != nil {
		unprocessableEntity(err.Error())
		return
	}

	// Error if there's not precisely one file upload field
	if len(r.MultipartForm.File) != 1 {
		unprocessableEntity("Expected one file upload")
		return
	}

	// Get the file
	file := r.MultipartForm.File["file"][0]

	// Error if the file bigger than 2GB
	if file.Size > 2*1024*1024*1024 {
		unprocessableEntity("File too large, must be less than 2GB")
		return
	}

	// Open the tmp uploaded file
	f, err := file.Open()
	if err != nil {
		internalServerError(err.Error())
		return
	}
	defer f.Close()

	// Remove .. and / from the filename to prevent directory traversal
	filename := strings.Replace(file.Filename, "..", "", -1)
	// Add current timestamp to the filename to prevent overwriting
	filename = time.Now().Format("20060102150405") + "-" + filename

	// Open a destination file
	destPath := destDir + "/" + filename
	dest, err := os.Create(destPath)
	if err != nil {
		internalServerError(err.Error())
		return
	}
	defer dest.Close()

	// Copy data from the uploaded file to the destination file
	if _, err := io.Copy(dest, f); err != nil {
		internalServerError(err.Error())
		return
	}

	created()
}

const form = `<form action="" method="post" enctype="multipart/form-data">
	<input type="file" name="file">
	<input type="submit" value="Upload">
</form>`

func getForm() {
	fmt.Println("Content-Type: text/html")
	fmt.Println("")
	fmt.Println("<!DOCTYPE html><html><body>")
	fmt.Println("<h1>Upload a file to Louis' server</h1>")
	fmt.Println(form)
	fmt.Println("</body></html>")
}

func created() {
	fmt.Println("Status: 201 Created")
	fmt.Println("Content-Type: text/html")
	fmt.Println("")
	fmt.Println("<!DOCTYPE html><html><body>")
	fmt.Println("<h1>File uploaded! Thank you</h1>")
	fmt.Println(form)
	fmt.Println("</body></html>")
}

func unprocessableEntity(msg string) {
	fmt.Println("Status: 422 Unprocessable Entity")
	fmt.Println("Content-Type: text/plain")
	fmt.Println("")
	fmt.Println("Unprocessable Entity: " + msg)
}

func internalServerError(msg string) {
	fmt.Println("Status: 500 Internal Server Error")
	fmt.Println("Content-Type: text/plain")
	fmt.Println("")
	fmt.Println("Internal server error: " + msg)
}
