variable "project_id" {
  description = "Google Cloud project ID. The default is intentionally non-routable for local validation."
  type        = string
  default     = "replace-with-gcp-project"
}

variable "region" {
  description = "Google Cloud region for Cloud Run, Scheduler, Artifact Registry, and Secret Manager."
  type        = string
  default     = "us-east1"
}

variable "enable_cloud_resources" {
  description = "Master opt-in. False creates no billable or remote Google Cloud resources."
  type        = bool
  default     = false
}

variable "enable_scheduler" {
  description = "Create a Cloud Scheduler trigger. Keep false during local evaluation."
  type        = bool
  default     = false
}

variable "publish_release" {
  description = "Pass --publish to the job. False ingests and stages a release for review."
  type        = bool
  default     = false
}

variable "container_image" {
  description = "Immutable Artifact Registry image reference, preferably pinned by digest."
  type        = string
  default     = "unused-while-enable-cloud-resources-is-false"
}

variable "retrosheet_season" {
  description = "Historical Retrosheet season handled by the introductory job."
  type        = number
  default     = 2025

  validation {
    condition     = var.retrosheet_season >= 1910 && var.retrosheet_season <= 2100
    error_message = "retrosheet_season must be between 1910 and 2100."
  }
}

variable "scheduler_cron" {
  description = "UTC schedule for the optional Retrosheet job. Releases are infrequent, so monthly is sufficient."
  type        = string
  default     = "15 11 1 * *"
}
