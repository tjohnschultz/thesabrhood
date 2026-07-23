locals {
  create_cloud_resources = var.enable_cloud_resources
  create_scheduler       = var.enable_cloud_resources && var.enable_scheduler
  job_args = concat(
    ["retrosheet", "--season=${var.retrosheet_season}"],
    var.publish_release ? ["--publish"] : []
  )
}

resource "google_project_service" "required" {
  for_each = local.create_cloud_resources ? toset([
    "artifactregistry.googleapis.com",
    "cloudscheduler.googleapis.com",
    "run.googleapis.com",
    "secretmanager.googleapis.com",
  ]) : toset([])

  project            = var.project_id
  service            = each.value
  disable_on_destroy = false
}

resource "google_artifact_registry_repository" "backend" {
  count = local.create_cloud_resources ? 1 : 0

  location      = var.region
  repository_id = "sabrhood-backend"
  description   = "Container images for The SABRhood data jobs"
  format        = "DOCKER"

  depends_on = [google_project_service.required]
}

resource "google_service_account" "job" {
  count = local.create_cloud_resources ? 1 : 0

  account_id   = "sabrhood-data-job"
  display_name = "The SABRhood data job"
}

resource "google_service_account" "scheduler" {
  count = local.create_scheduler ? 1 : 0

  account_id   = "sabrhood-scheduler"
  display_name = "The SABRhood Scheduler invoker"
}

resource "google_secret_manager_secret" "database_url" {
  count = local.create_cloud_resources ? 1 : 0

  secret_id = "sabrhood-database-url"
  replication {
    auto {}
  }

  depends_on = [google_project_service.required]
}

resource "google_secret_manager_secret_iam_member" "job_database_url" {
  count = local.create_cloud_resources ? 1 : 0

  secret_id = google_secret_manager_secret.database_url[0].id
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.job[0].email}"
}

resource "google_cloud_run_v2_job" "retrosheet" {
  count = local.create_cloud_resources ? 1 : 0

  name                = "sabrhood-retrosheet"
  location            = var.region
  deletion_protection = true

  template {
    template {
      service_account = google_service_account.job[0].email
      max_retries     = 1
      timeout         = "3600s"

      containers {
        image = var.container_image
        args  = local.job_args

        resources {
          limits = {
            cpu    = "2"
            memory = "4Gi"
          }
        }

        env {
          name = "DATABASE_URL"
          value_source {
            secret_key_ref {
              secret  = google_secret_manager_secret.database_url[0].secret_id
              version = "latest"
            }
          }
        }

        env {
          name  = "TRIGGER_KIND"
          value = local.create_scheduler ? "scheduled" : "manual"
        }
      }
    }
  }

  depends_on = [
    google_project_service.required,
    google_secret_manager_secret_iam_member.job_database_url,
  ]
}

resource "google_cloud_run_v2_job_iam_member" "scheduler_invoker" {
  count = local.create_scheduler ? 1 : 0

  project  = var.project_id
  location = google_cloud_run_v2_job.retrosheet[0].location
  name     = google_cloud_run_v2_job.retrosheet[0].name
  role     = "roles/run.invoker"
  member   = "serviceAccount:${google_service_account.scheduler[0].email}"
}

resource "google_cloud_scheduler_job" "retrosheet" {
  count = local.create_scheduler ? 1 : 0

  name             = "sabrhood-retrosheet"
  description      = "Run the staged Retrosheet historical ingestion job"
  region           = var.region
  schedule         = var.scheduler_cron
  time_zone        = "Etc/UTC"
  attempt_deadline = "320s"

  retry_config {
    retry_count          = 2
    min_backoff_duration = "30s"
    max_backoff_duration = "300s"
    max_doublings        = 2
  }

  http_target {
    http_method = "POST"
    uri = format(
      "https://run.googleapis.com/v2/projects/%s/locations/%s/jobs/%s:run",
      var.project_id,
      google_cloud_run_v2_job.retrosheet[0].location,
      google_cloud_run_v2_job.retrosheet[0].name
    )
    body = base64encode("{}")

    headers = {
      "Content-Type" = "application/json"
    }

    oauth_token {
      service_account_email = google_service_account.scheduler[0].email
    }
  }

  depends_on = [
    google_project_service.required,
    google_cloud_run_v2_job_iam_member.scheduler_invoker,
  ]
}
