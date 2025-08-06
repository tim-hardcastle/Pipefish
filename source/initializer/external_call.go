package initializer

// Just a function and a couple of types for making an external service call, used by
// `api_serialization.go` in this package to construct an external service.

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"

	"github.com/tim-hardcastle/Pipefish/source/settings"
)

type jsonRequest = struct {
	Body     string
	Username string
	Password string
}

type jsonResponse = struct {
	Body    string
	Service string
}

func Do(host, line, username, password string) string {
	jRq := jsonRequest{Body: line, Username: username, Password: password}
	body, _ := json.Marshal(jRq)
	request, err := http.NewRequest("POST", host, bytes.NewBuffer(body))
	if err != nil {
		return "error \"Can't parse request\"" // Obviously this one shouldn't happen.
	}
	request.Header.Set("Content-Type", "application/json; charset=UTF-8")

	client := &http.Client{}
	response, err := client.Do(request)
	if err != nil {
		return "error \"Can't get response from '" + host + "'\""
	}

	defer response.Body.Close()
	rBody, _ := io.ReadAll(response.Body)
	if settings.SHOW_XCALLS {
		rawJ := ""
		for _, c := range rBody {
			rawJ = rawJ + (string(c))
		}
		println("Raw json is", rawJ)
	}
	var jRsp jsonResponse
	json.Unmarshal(rBody, &jRsp)
	return jRsp.Body
}
