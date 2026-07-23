# Optional Google Cloud infrastructure

The configuration is inert by default. With
`enable_cloud_resources = false`, it creates no remote or billable resources.
It exists so account setup and costs can be reviewed before the first apply.

When deliberately enabled it creates:

- an Artifact Registry Docker repository;
- a least-privilege Cloud Run job identity;
- a Secret Manager secret container for `DATABASE_URL` (not the secret value);
- one Cloud Run Job for staged Retrosheet ingestion; and
- optionally, a Cloud Scheduler job that invokes the Cloud Run Jobs API with
  OAuth.

Both `publish_release` and `enable_scheduler` default to false.

## Safe evaluation

```powershell
terraform init
terraform fmt -check
terraform validate
terraform plan
```

The default plan should report no remote resources to add. Do not set the
master opt-in until the prerequisites in
[`../ROLLOUT.md`](../ROLLOUT.md) are complete.

The database secret value must be added out-of-band after apply:

```powershell
gcloud secrets versions add sabrhood-database-url --data-file=-
```

Never put the connection string in `terraform.tfvars`, Terraform state, an
image, or source control.

