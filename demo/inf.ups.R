dat.forum <- get.inf.data.hourly(from="2012-12-10", to="2012-12-16", upss=c("forumA", "forumB"))
plot(dat.forum)

dat.server <- get.inf.data.hourly(from="2012-12-10", to="2012-12-16", upss=c("serverL", "serverR"))
plot(dat.server)

dat.combined <- combine.data.hourly(list(forum=dat.forum, server=dat.server))
plot(dat.combined)
