package transcode

import "fmt"

type TranscodeProfile = string
type InFile = string
type OutFile = string
type OutMetadata = string

type Service interface {
	TranscodeVideo(InFile, OutFile, TranscodeProfile) (OutMetadata, error)
}

// Transcode service that actually transcodes

type LiveService struct{}

func (service LiveService) TranscodeVideo(InFile, OutFile, TranscodeProfile) (OutMetadata, error) {
	// TODO: transcode video here
	fmt.Println("LiveService.TranscodeVideo called")
	return "some metadata", nil
}

// Transcode service that pretends to transcode, for tests

type InertService struct{}

func (service InertService) TranscodeVideo(InFile, OutFile, TranscodeProfile) (OutMetadata, error) {
	return "some metadata", nil
}
