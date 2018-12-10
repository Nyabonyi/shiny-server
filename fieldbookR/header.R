header <- dashboardHeader(
  title = "Data Portal",
  dropdownMenuOutput("msgOutput"),
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "text here: two new tasks added",
      icon = icon("dashboard"),
      status = "success"
    ),
    notificationItem(
      text = "server is currently running at 95% load",
      icon = icon("warning"),
      status = "warning"
    )
  ),
  dropdownMenu(
    type = "tasks",
    taskItem(
      value = 80,
      color = "aqua",
      text = "The project name or task name"
    ),
    taskItem(
      value = 50,
      color = "green",
      text = "Task text for task2"
    )
  )
)




