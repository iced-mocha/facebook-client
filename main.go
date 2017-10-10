package main

import (
	"encoding/json"
	"net/http"
	"log"

	"github.com/gorilla/mux"
	"github.com/iced-mocha/shared/models"
)

func main() {
	r := mux.NewRouter()
	r.HandleFunc("/v1/posts", GetPosts).Methods("GET")
	log.Fatal(http.ListenAndServe(":5000", r))
}

func GetPosts(w http.ResponseWriter, r *http.Request) {
	posts := make([]models.Post, 20)
	posts = append(posts, models.Post{})
	res, err := json.Marshal(posts)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(res)
}
