package main

import (
	"brook/internal/pkg/database"
	"brook/internal/pkg/transcode"
	"brook/internal/pkg/workflow"
)

func main() {
	rootPointer, err := database.GetWorkflowRoot(database.SimpleTranscodeWorkflowId)
	if err != nil {
		panic(err)
	}

	transcodeService := transcode.LiveService{}

	visitor := workflow.VisitorExec{transcodeService}

	root := *rootPointer
	root.Visit(visitor)
}
