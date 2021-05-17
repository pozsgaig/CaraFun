zot_read_item_G<-
function (id, user = NULL, credentials = NULL, format=NULL, include=NULL) 
{
  require(zoteror)
  get_call<-paste0("https://api.zotero.org/users/", 
                    user, "/items/", id, "?key=", credentials, "&format=", format, "&include=", include)
  print(get_call)
  jsonlite::fromJSON(txt = get_call)
}

#https://api.zotero.org/users/1234567/items/YYYYYY?key=xxxxxxxxx&format=json&include=citation,biblatex