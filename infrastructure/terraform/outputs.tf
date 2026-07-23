output "cloud_resources_enabled" {
  description = "Confirms whether this configuration is allowed to create remote resources."
  value       = var.enable_cloud_resources
}

output "artifact_registry_repository" {
  description = "Artifact Registry repository path when cloud resources are enabled."
  value = local.create_cloud_resources ? format(
    "%s-docker.pkg.dev/%s/%s",
    var.region,
    var.project_id,
    google_artifact_registry_repository.backend[0].repository_id
  ) : null
}

output "cloud_run_job_name" {
  description = "Cloud Run job name when enabled."
  value       = local.create_cloud_resources ? google_cloud_run_v2_job.retrosheet[0].name : null
}

output "database_url_secret_id" {
  description = "Secret Manager secret to populate out-of-band; Terraform never stores the database URL value."
  value       = local.create_cloud_resources ? google_secret_manager_secret.database_url[0].secret_id : null
}

