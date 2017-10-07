package main

import (
	"encoding/json"
	"log"
	"net/http"
	"strconv"
	"time"

	"github.com/gorilla/mux"
	"github.com/iced-mocha/shared/models"
)

func main() {
	r := mux.NewRouter()
	r.HandleFunc("/v1/posts", GetPosts).Methods("GET")
	log.Fatal(http.ListenAndServe(":5000", r))
}

func GetPosts(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.Write(nil)
}
