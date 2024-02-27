function get_id(clicked_id) {
  Shiny.setInputValue("home-current_id", clicked_id, {priority: "event"});
}
