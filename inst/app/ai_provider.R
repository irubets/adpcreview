# Provider-neutral adapter for OpenAI GPT and local Ollama models.

.ai_provider_envs <- new.env(parent = emptyenv())
.ai_provider_envs$gpt <- new.env(parent = globalenv())
.ai_provider_envs$qwen <- new.env(parent = globalenv())
.ai_provider_envs$anthropic <- new.env(parent = globalenv())

sys.source(file.path(.app_dir, "gpt_assistant.R"), envir = .ai_provider_envs$gpt)
sys.source(file.path(.app_dir, "qwen_assistant.R"), envir = .ai_provider_envs$qwen)
sys.source(file.path(.app_dir, "anthropic_assistant.R"), envir = .ai_provider_envs$anthropic)

ai_provider_normalize <- function(provider) {
  provider <- tolower(trimws(provider %||% "qwen"))
  if (!provider %in% c("qwen", "gpt", "anthropic")) "qwen" else provider
}

ai_provider_label <- function(provider) {
  provider <- ai_provider_normalize(provider)
  if (provider == "gpt") "OpenAI GPT" else if (provider == "anthropic") "Anthropic Claude" else "local Qwen (Ollama)"
}

ai_provider_config <- function(provider) {
  env <- .ai_provider_envs[[ai_provider_normalize(provider)]]
  env$ai_config()
}

ai_provider_status <- function(provider, model = NULL) {
  provider <- ai_provider_normalize(provider)
  env <- .ai_provider_envs[[provider]]
  if (provider == "gpt") env$ai_openai_status(model) else
    if (provider == "anthropic") env$ai_anthropic_status(model) else env$ai_ollama_status(model)
}

ai_provider_call <- function(provider, question, context, history = list(), model = NULL,
                             reasoning_effort = "low") {
  provider <- ai_provider_normalize(provider)
  env <- .ai_provider_envs[[provider]]
  if (provider == "gpt") {
    env$ai_call_openai(question, context, history, model, reasoning_effort)
  } else if (provider == "anthropic") {
    env$ai_call_anthropic(question, context, history, model, reasoning_effort)
  } else {
    env$ai_call_ollama(question, context, history, model, reasoning_effort)
  }
}

ai_provider_plan_data_query <- function(provider, question, data, model = NULL,
                                        reasoning_effort = "low") {
  env <- .ai_provider_envs[[ai_provider_normalize(provider)]]
  env$ai_plan_data_query(question, data, model, reasoning_effort)
}

ai_provider_generate_visualization <- function(provider, request, context, available_columns,
                                               model = NULL, reasoning_effort = "low") {
  env <- .ai_provider_envs[[ai_provider_normalize(provider)]]
  env$ai_generate_visualization(request, context, available_columns, model, reasoning_effort)
}

# These helpers are provider-independent. Export one canonical implementation for
# the main app while keeping each provider's call stack isolated in its own env.
ai_review_context <- .ai_provider_envs$qwen$ai_review_context
validate_visualization_code_v12 <- .ai_provider_envs$qwen$validate_visualization_code_v12
