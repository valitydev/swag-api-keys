%% -*- mode: erlang -*-
-module(oas_client_schema).

-export([get/0]).
-export([get_raw/0]).
-export([enumerate_discriminator_children/1]).

-define(COMPONENTS, <<"components">>).
-define(SCHEMAS, <<"schemas">>).

-spec get() -> oas_client:object().
get() ->
    ct_expand:term(enumerate_discriminator_children(get_raw())).

-spec enumerate_discriminator_children(Schema :: map()) ->
    Schema :: map() | no_return().
enumerate_discriminator_children(Schema) ->
    try
        ComponentSchemas = maps:get(?SCHEMAS, maps:get(?COMPONENTS, Schema)),
        Parents = enumerate_parents(ComponentSchemas),
        ComponentSchemasFixed = maps:fold(fun correct_definition/3, ComponentSchemas, Parents),
        Schema#{?COMPONENTS => #{?SCHEMAS => ComponentSchemasFixed}}
    catch
        _:Error ->
            handle_error(Error)
    end.

-spec handle_error(_) ->
    no_return().
handle_error(Error) ->
    erlang:error({schema_invalid, Error}).

enumerate_parents(Schemas) ->
    maps:fold(
        fun
            (Name, #{<<"allOf">> := AllOf}, AccIn) ->
                lists:foldl(
                    fun
                        (#{<<"$ref">> := <<"#/components/schemas/", Parent/binary>>}, Acc) ->
                            Schema = maps:get(Parent, Schemas),
                            Discriminator = maps:get(<<"discriminator">>, Schema, undefined),
                            add_parent_child(Discriminator, Parent, Name, Acc);
                        (_Schema, Acc) ->
                            Acc
                    end,
                    AccIn,
                    AllOf
                );
            (Name, #{<<"discriminator">> := _}, Acc) ->
                add_parent(Name, Acc);
            (_Name, _Schema, AccIn) ->
                AccIn
        end,
        #{},
        Schemas
    ).

add_parent_child(undefined, _Parent, _Child, Acc) ->
    Acc;
add_parent_child(_Discriminator, Parent, Child, Acc) ->
    maps:put(Parent, [Child | maps:get(Parent, Acc, [])], Acc).

add_parent(Parent, Acc) when not is_map_key(Parent, Acc) ->
    maps:put(Parent, [], Acc);
add_parent(_Parent, Acc) ->
    Acc.

correct_definition(Parent, Children, Definitions) ->
    ParentSchema1 = maps:get(Parent, Definitions),
    Discriminator = maps:get(<<"propertyName">>, maps:get(<<"discriminator">>, ParentSchema1)),
    ParentSchema2 = deep_put([<<"properties">>, Discriminator, <<"enum">>], Children, ParentSchema1),
    maps:put(Parent, ParentSchema2, Definitions).

deep_put([K], V, M) ->
    M#{K => V};
deep_put([K | Ks], V, M) ->
    maps:put(K, deep_put(Ks, V, maps:get(K, M)), M).

-spec get_raw() -> map().
get_raw() ->
    #{
  <<"openapi">> => <<"3.0.3">>,
  <<"info">> => #{
    <<"title">> => <<"Vality API Keys Management API">>,
    <<"description">> => <<"Vality API Keys Management API является интерфейсом для управления набором\nAPI-ключей, используемых для авторизации запросов к основному API с ваших\nбэкенд-сервисов. Любые сторонние приложения, включая ваш личный кабинет,\nявляются внешними приложениями-клиентами данного API.\n\nМы предоставляем REST API поверх HTTP-протокола, схема которого описывается в\nсоответствии со стандартом [OpenAPI 3][OAS3].\nКоды возврата описываются соответствующими HTTP-статусами. Платформа принимает и\nвозвращает значения JSON в теле запросов и ответов.\n\n[OAS3]: https://swagger.io/specification/\n\n## Идентификатор запроса\n\nПри любом обращении к API в заголовке `X-Request-ID` соответствующего запроса необходимо\nпередать его уникальный идентификатор:\n\n```\n    X-Request-ID: 37d735d4-0f42-4f05-89fa-eaa478fb5aa9\n```\n\n## Формат содержимого\n\nЛюбой запрос к API должен выполняться в кодировке UTF-8 и с указанием\nсодержимого в формате JSON.\n\n```\nContent-Type: application/json; charset=utf-8\n```\n\n## Максимальное время обработки запроса\n\nПри любом обращении к API в заголовке `X-Request-Deadline` соответствующего запроса можно\nпередать параметр отсечки по времени, определяющий максимальное время ожидания завершения\nоперации по запросу:\n\n```\n    X-Request-Deadline: 10s\n```\n\nПо истечении указанного времени система прекращает обработку запроса. Рекомендуется указывать\nзначение не более одной минуты, но не менее трёх секунд.\n\n`X-Request-Deadline` может:\n\n* задаваться в формате `date-time` согласно\n    [RFC 3339](https://datatracker.ietf.org/doc/html/rfc3339);\n* задаваться в относительных величинах: в миллисекундах (`150000ms`), секундах (`540s`) или\n    минутах (`3.5m`).\n">>,
    <<"termsOfService">> => <<"https://vality.dev/">>,
    <<"license">> => #{
      <<"name">> => <<"Apache 2.0">>,
      <<"url">> => <<"https://www.apache.org/licenses/LICENSE-2.0.html">>
    },
    <<"version">> => <<"1.0.0">>
  },
  <<"servers">> => [ #{
    <<"url">> => <<"/">>
  } ],
  <<"security">> => [ #{
    <<"bearer">> => [ ]
  } ],
  <<"tags">> => [ #{
    <<"name">> => <<"apiKeys">>,
    <<"x-displayName">> => <<"API-ключи">>
  }, #{
    <<"name">> => <<"errorCodes">>,
    <<"description">> => <<"## Общие ошибки\n\nОшибки возникающие при попытках совершения недопустимых операций, операций с невалидными объектами или несуществующими ресурсами. Имеют следующий вид:\n\n```json\n{\n    \"code\": \"string\",\n    \"message\": \"string\"\n}\n```\n\nВ поле `message` содержится информация по произошедшей ошибке. Например:\n\n```json\n{\n    \"code\": \"invalidRequest\",\n    \"message\": \"Property 'name' is required.\"\n}\n```\n\n## Ошибки обработки запросов\n\nВ процессе обработки запросов силами нашей платформы могут происходить различные непредвиденные ситуации. Об их появлении платформа сигнализирует по протоколу HTTP соответствующими [статусами][5xx], обозначающими ошибки сервера.\n\n|  Код    |  Описание  |\n| ------- | ---------- |\n| **500** | В процессе обработки платформой запроса возникла непредвиденная ситуация. При получении подобного кода ответа мы рекомендуем обратиться в техническую поддержку. |\n| **503** | Платформа временно недоступна и не готова обслуживать данный запрос. Запрос гарантированно не выполнен, при получении подобного кода ответа попробуйте выполнить его позднее, когда доступность платформы будет восстановлена. |\n| **504** | Платформа превысила допустимое время обработки запроса, результат запроса не определён. Попробуйте отправить запрос повторно или выяснить результат выполнения исходного запроса, если повторное исполнение запроса нежелательно. |\n\n[5xx]: https://tools.ietf.org/html/rfc7231#section-6.6\n\n\nЕсли вы получили ошибку, которой нет в данном описании, обратитесь в техническую поддержку.\n">>,
    <<"x-displayName">> => <<"Коды ошибок">>
  } ],
  <<"paths">> => #{
    <<"/orgs/{partyId}/api-keys">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Перечислить ключи организации">>,
        <<"operationId">> => <<"listApiKeys">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 32,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
            <<"example">> => <<"30672daac16a1f3c5e770a5a09626d1f">>
          }
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Максимальное время обработки запроса">>,
            <<"example">> => <<"10s">>
          }
        }, #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор участника">>,
            <<"example">> => <<"bdaf9e76-1c5b-4798-b154-19b87a61dc94">>
          }
        }, #{
          <<"name">> => <<"status">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Фильтр по статусу ключа. По умолчанию `active`.\n">>,
          <<"required">> => false,
          <<"style">> => <<"form">>,
          <<"explode">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/components/schemas/ApiKeyStatus">>
          }
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Ключи найдены">>,
            <<"content">> => #{
              <<"application/json">> => #{
                <<"schema">> => #{
                  <<"$ref">> => <<"#/components/schemas/inline_response_200">>
                }
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Переданы ошибочные данные">>,
            <<"content">> => #{
              <<"application/json">> => #{
                <<"schema">> => #{
                  <<"$ref">> => <<"#/components/schemas/inline_response_400">>
                }
              }
            }
          },
          <<"403">> => #{
            <<"description">> => <<"Операция недоступна">>
          }
        }
      },
      <<"post">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Выпустить новый ключ">>,
        <<"operationId">> => <<"issueApiKey">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор участника">>,
            <<"example">> => <<"bdaf9e76-1c5b-4798-b154-19b87a61dc94">>
          }
        } ],
        <<"requestBody">> => #{
          <<"content">> => #{
            <<"application/json">> => #{
              <<"schema">> => #{
                <<"$ref">> => <<"#/components/schemas/ApiKey">>
              }
            }
          }
        },
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Ключ выпущен">>,
            <<"content">> => #{
              <<"application/json">> => #{
                <<"schema">> => #{
                  <<"$ref">> => <<"#/components/schemas/inline_response_200_1">>
                }
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Переданы ошибочные данные">>,
            <<"content">> => #{
              <<"application/json">> => #{
                <<"schema">> => #{
                  <<"$ref">> => <<"#/components/schemas/inline_response_400">>
                }
              }
            }
          },
          <<"403">> => #{
            <<"description">> => <<"Операция недоступна">>
          }
        }
      }
    },
    <<"/orgs/{partyId}/api-keys/{apiKeyId}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Получить данные ключа">>,
        <<"operationId">> => <<"getApiKey">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 32,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
            <<"example">> => <<"30672daac16a1f3c5e770a5a09626d1f">>
          }
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Максимальное время обработки запроса">>,
            <<"example">> => <<"10s">>
          }
        }, #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор участника">>,
            <<"example">> => <<"bdaf9e76-1c5b-4798-b154-19b87a61dc94">>
          }
        }, #{
          <<"name">> => <<"apiKeyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор ключа">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"$ref">> => <<"#/components/schemas/ApiKeyID">>
          }
        } ],
        <<"responses">> => #{
          <<"200">> => #{
            <<"description">> => <<"Ключ найден">>,
            <<"content">> => #{
              <<"application/json">> => #{
                <<"schema">> => #{
                  <<"$ref">> => <<"#/components/schemas/ApiKey">>
                }
              }
            }
          },
          <<"400">> => #{
            <<"description">> => <<"Переданы ошибочные данные">>,
            <<"content">> => #{
              <<"application/json">> => #{
                <<"schema">> => #{
                  <<"$ref">> => <<"#/components/schemas/inline_response_400">>
                }
              }
            }
          },
          <<"403">> => #{
            <<"description">> => <<"Операция недоступна">>
          },
          <<"404">> => #{
            <<"description">> => <<"Ключ не найден">>
          }
        }
      }
    },
    <<"/orgs/{partyId}/api-keys/{apiKeyId}/status">> => #{
      <<"put">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Запросить отзыв ключа">>,
        <<"description">> => <<"Просит отозвать Api Key, для подтверждения запроса\nпосылает на почту запросившего письмо с ссылкой на\nrevokeApiKey для подтверждения операции\n">>,
        <<"operationId">> => <<"requestRevokeApiKey">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 32,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
            <<"example">> => <<"30672daac16a1f3c5e770a5a09626d1f">>
          }
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Максимальное время обработки запроса">>,
            <<"example">> => <<"10s">>
          }
        }, #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор участника">>,
            <<"example">> => <<"bdaf9e76-1c5b-4798-b154-19b87a61dc94">>
          }
        }, #{
          <<"name">> => <<"apiKeyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор ключа">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"$ref">> => <<"#/components/schemas/ApiKeyID">>
          }
        } ],
        <<"requestBody">> => #{
          <<"content">> => #{
            <<"application/json">> => #{
              <<"schema">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [ <<"Revoked">> ]
              }
            }
          }
        },
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Запрос на операцию получен">>
          },
          <<"400">> => #{
            <<"description">> => <<"Переданы ошибочные данные">>,
            <<"content">> => #{
              <<"application/json">> => #{
                <<"schema">> => #{
                  <<"$ref">> => <<"#/components/schemas/inline_response_400">>
                }
              }
            }
          },
          <<"403">> => #{
            <<"description">> => <<"Операция недоступна">>
          },
          <<"404">> => #{
            <<"description">> => <<"Ключ не найден">>
          }
        }
      }
    },
    <<"/orgs/{partyId}/revoke-api-key/{apiKeyId}">> => #{
      <<"get">> => #{
        <<"tags">> => [ <<"apiKeys">> ],
        <<"summary">> => <<"Отозвать ключ">>,
        <<"description">> => <<"Ссылка на этот запрос приходит на почту запросившего\nrequestRevokeApiKey, в результате выполнения этого запроса\nApi Key будет отозван\n">>,
        <<"operationId">> => <<"revokeApiKey">>,
        <<"parameters">> => [ #{
          <<"name">> => <<"X-Request-ID">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 32,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
            <<"example">> => <<"30672daac16a1f3c5e770a5a09626d1f">>
          }
        }, #{
          <<"name">> => <<"X-Request-Deadline">>,
          <<"in">> => <<"header">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"required">> => false,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Максимальное время обработки запроса">>,
            <<"example">> => <<"10s">>
          }
        }, #{
          <<"name">> => <<"partyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Идентификатор участника">>,
            <<"example">> => <<"bdaf9e76-1c5b-4798-b154-19b87a61dc94">>
          }
        }, #{
          <<"name">> => <<"apiKeyId">>,
          <<"in">> => <<"path">>,
          <<"description">> => <<"Идентификатор ключа">>,
          <<"required">> => true,
          <<"style">> => <<"simple">>,
          <<"explode">> => false,
          <<"schema">> => #{
            <<"$ref">> => <<"#/components/schemas/ApiKeyID">>
          }
        }, #{
          <<"name">> => <<"apiKeyRevokeToken">>,
          <<"in">> => <<"query">>,
          <<"description">> => <<"Токен отзыва ключа">>,
          <<"required">> => true,
          <<"style">> => <<"form">>,
          <<"explode">> => true,
          <<"schema">> => #{
            <<"$ref">> => <<"#/components/schemas/RevokeToken">>
          }
        } ],
        <<"responses">> => #{
          <<"204">> => #{
            <<"description">> => <<"Ключ отозван">>
          },
          <<"400">> => #{
            <<"description">> => <<"Переданы ошибочные данные">>,
            <<"content">> => #{
              <<"application/json">> => #{
                <<"schema">> => #{
                  <<"$ref">> => <<"#/components/schemas/inline_response_400">>
                }
              }
            }
          },
          <<"403">> => #{
            <<"description">> => <<"Операция недоступна">>
          },
          <<"404">> => #{
            <<"description">> => <<"Ключ не найден">>
          }
        }
      }
    }
  },
  <<"components">> => #{
    <<"schemas">> => #{
      <<"ApiKeyStatus">> => #{
        <<"type">> => <<"string">>,
        <<"description">> => <<"Статус ключа">>,
        <<"readOnly">> => true,
        <<"enum">> => [ <<"Active">>, <<"Revoked">> ]
      },
      <<"ApiKeyID">> => #{
        <<"maxLength">> => 40,
        <<"minLength">> => 1,
        <<"type">> => <<"string">>,
        <<"description">> => <<"Идентификатор ключа">>,
        <<"readOnly">> => true,
        <<"example">> => <<"1KgIYBGsCgq">>
      },
      <<"ApiKey">> => #{
        <<"required">> => [ <<"createdAt">>, <<"id">>, <<"name">>, <<"status">> ],
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"id">> => #{
            <<"$ref">> => <<"#/components/schemas/ApiKeyID">>
          },
          <<"createdAt">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"Дата и время создания">>,
            <<"format">> => <<"date-time">>,
            <<"readOnly">> => true
          },
          <<"name">> => #{
            <<"maxLength">> => 40,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Запоминающееся название ключа">>,
            <<"example">> => <<"live-site-integration">>
          },
          <<"status">> => #{
            <<"$ref">> => <<"#/components/schemas/ApiKeyStatus">>
          },
          <<"metadata">> => #{
            <<"type">> => <<"object">>,
            <<"description">> => <<"Произвольный набор данных, специфичный для клиента API и\nнепрозрачный для системы\n">>
          }
        },
        <<"description">> => <<"Ключ для авторизации запросов к API">>,
        <<"example">> => #{
          <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
          <<"metadata">> => #{ },
          <<"name">> => <<"live-site-integration">>,
          <<"id">> => <<"1KgIYBGsCgq">>,
          <<"status">> => <<"Active">>
        }
      },
      <<"AccessToken">> => #{
        <<"required">> => [ <<"accessToken">> ],
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"accessToken">> => #{
            <<"maxLength">> => 4000,
            <<"minLength">> => 1,
            <<"type">> => <<"string">>,
            <<"description">> => <<"Токен доступа, ассоциированный с данным ключом">>,
            <<"example">> => <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0a2kiOiIxS2dJWUJHc0NncSIsImlhdCI6MTUxNjIzOTAyMn0.6YsaZQC9A7BjxXHwRbJfUO6VujOb4rHTKrqmMt64TbQ\n">>
          }
        }
      },
      <<"RevokeToken">> => #{
        <<"maxLength">> => 4000,
        <<"minLength">> => 1,
        <<"type">> => <<"string">>,
        <<"description">> => <<"Токен отзыва ключа, приходит с ссылкой в почте">>,
        <<"example">> => <<"f767b77e-300f-47a7-84e2-e24ea585a9f0\n">>
      },
      <<"inline_response_200">> => #{
        <<"required">> => [ <<"results">> ],
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"results">> => #{
            <<"type">> => <<"array">>,
            <<"items">> => #{
              <<"$ref">> => <<"#/components/schemas/ApiKey">>
            }
          }
        },
        <<"example">> => #{
          <<"results">> => [ #{
            <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
            <<"metadata">> => #{ },
            <<"name">> => <<"live-site-integration">>,
            <<"id">> => <<"1KgIYBGsCgq">>,
            <<"status">> => <<"Active">>
          }, #{
            <<"createdAt">> => <<"2000-01-23T04:56:07.000+00:00">>,
            <<"metadata">> => #{ },
            <<"name">> => <<"live-site-integration">>,
            <<"id">> => <<"1KgIYBGsCgq">>,
            <<"status">> => <<"Active">>
          } ]
        }
      },
      <<"inline_response_400">> => #{
        <<"required">> => [ <<"code">> ],
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"code">> => #{
            <<"type">> => <<"string">>,
            <<"enum">> => [ <<"invalidRequest">> ]
          },
          <<"message">> => #{
            <<"type">> => <<"string">>
          }
        },
        <<"description">> => <<"Ошибка в переданных данных">>
      },
      <<"inline_response_200_1">> => #{
        <<"allOf">> => [ #{
          <<"$ref">> => <<"#/components/schemas/ApiKey">>
        }, #{
          <<"$ref">> => <<"#/components/schemas/AccessToken">>
        } ]
      }
    },
    <<"responses">> => #{
      <<"BadRequest">> => #{
        <<"description">> => <<"Переданы ошибочные данные">>,
        <<"content">> => #{
          <<"application/json">> => #{
            <<"schema">> => #{
              <<"$ref">> => <<"#/components/schemas/inline_response_400">>
            }
          }
        }
      }
    },
    <<"parameters">> => #{
      <<"requestID">> => #{
        <<"name">> => <<"X-Request-ID">>,
        <<"in">> => <<"header">>,
        <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
        <<"required">> => true,
        <<"style">> => <<"simple">>,
        <<"explode">> => false,
        <<"schema">> => #{
          <<"maxLength">> => 32,
          <<"minLength">> => 1,
          <<"type">> => <<"string">>,
          <<"description">> => <<"Уникальный идентификатор запроса к системе">>,
          <<"example">> => <<"30672daac16a1f3c5e770a5a09626d1f">>
        }
      },
      <<"deadline">> => #{
        <<"name">> => <<"X-Request-Deadline">>,
        <<"in">> => <<"header">>,
        <<"description">> => <<"Максимальное время обработки запроса">>,
        <<"required">> => false,
        <<"style">> => <<"simple">>,
        <<"explode">> => false,
        <<"schema">> => #{
          <<"maxLength">> => 40,
          <<"minLength">> => 1,
          <<"type">> => <<"string">>,
          <<"description">> => <<"Максимальное время обработки запроса">>,
          <<"example">> => <<"10s">>
        }
      },
      <<"partyId">> => #{
        <<"name">> => <<"partyId">>,
        <<"in">> => <<"path">>,
        <<"description">> => <<"Идентификатор участника">>,
        <<"required">> => true,
        <<"style">> => <<"simple">>,
        <<"explode">> => false,
        <<"schema">> => #{
          <<"maxLength">> => 40,
          <<"minLength">> => 1,
          <<"type">> => <<"string">>,
          <<"description">> => <<"Идентификатор участника">>,
          <<"example">> => <<"bdaf9e76-1c5b-4798-b154-19b87a61dc94">>
        }
      },
      <<"apiKeyId">> => #{
        <<"name">> => <<"apiKeyId">>,
        <<"in">> => <<"path">>,
        <<"description">> => <<"Идентификатор ключа">>,
        <<"required">> => true,
        <<"style">> => <<"simple">>,
        <<"explode">> => false,
        <<"schema">> => #{
          <<"$ref">> => <<"#/components/schemas/ApiKeyID">>
        }
      },
      <<"apiKeyRevokeToken">> => #{
        <<"name">> => <<"apiKeyRevokeToken">>,
        <<"in">> => <<"query">>,
        <<"description">> => <<"Токен отзыва ключа">>,
        <<"required">> => true,
        <<"style">> => <<"form">>,
        <<"explode">> => true,
        <<"schema">> => #{
          <<"$ref">> => <<"#/components/schemas/RevokeToken">>
        }
      }
    },
    <<"securitySchemes">> => #{
      <<"bearer">> => #{
        <<"type">> => <<"http">>,
        <<"description">> => <<"Для аутентификации вызовов мы используем [JWT](https://jwt.io). Токен доступа передается в заголовке.\n```shell\n Authorization: Bearer {JWT}\n```\nЗапросы к данному API авторизуются сессионным токеном доступа, который вы получаете в результате аутентификации в личном кабинете.\n">>,
        <<"scheme">> => <<"bearer">>,
        <<"bearerFormat">> => <<"JWT">>
      }
    }
  }
}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SCHEMA,
  <<"{\"components\": {
    \"schemas\": {
       \"Pet\": {
         \"type\":          \"object\",
         \"discriminator\": {\"propertyName\": \"petType\"},
         \"properties\": {
            \"name\":    {\"type\": \"string\"},
            \"petType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"petType\"]
       },
       \"Cat\": {
         \"description\": \"A representation of a cat\",
         \"allOf\": [
           {\"$ref\": \"#/components/schemas/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"huntingSkill\": {
                 \"type\":        \"string\",
                 \"description\": \"The measured skill for hunting\",
                 \"default\":     \"lazy\",
                 \"enum\":        [\"clueless\", \"lazy\", \"adventurous\", \"aggressive\"]
               }
             },
             \"required\": [\"huntingSkill\"]
           }
         ]
       },
       \"Dog\": {
         \"description\": \"A representation of a dog\",
         \"allOf\": [
           {\"$ref\": \"#/components/schemas/Pet\"},
           {
             \"type\":       \"object\",
             \"properties\": {
               \"packSize\": {
                 \"type\":        \"integer\",
                 \"format\":      \"int32\",
                 \"description\": \"the size of the pack the dog is from\",
                 \"default\":     0,
                 \"minimum\":     0
               }
             }
           }
         ],
         \"required\": [\"packSize\"]
       },
       \"Person\": {
         \"type\":          \"object\",
         \"discriminator\": {\"propertyName\": \"personType\"},
         \"properties\": {
           \"name\": {\"type\": \"string\"},
           \"sex\": {
             \"type\": \"string\",
             \"enum\": [\"male\", \"female\"]
           },
           \"personType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"sex\", \"personType\"]
       },
       \"WildMix\": {
         \"allOf\": [
           {\"$ref\": \"#/components/schemas/Pet\"},
           {\"$ref\": \"#/components/schemas/Person\"}
         ],
       },
       \"Dummy\": {
         \"type\":          \"object\",
         \"discriminator\": {\"propertyName\": \"dummyType\"},
         \"properties\": {
           \"name\":      {\"type\": \"string\"},
           \"dummyType\": {\"type\": \"string\"}
         },
         \"required\": [\"name\", \"dummyType\"]
       }
     }}}">>).

get_enum(Parent, Discr, Schema) ->
    lists:sort(deep_get([?COMPONENTS, ?SCHEMAS, Parent, <<"properties">>, Discr, <<"enum">>], Schema)).

deep_get([K], M) ->
    maps:get(K, M);
deep_get([K | Ks], M) ->
    deep_get(Ks, maps:get(K, M)).

-spec test() -> _.
-spec enumerate_discriminator_children_test() -> _.
enumerate_discriminator_children_test() ->
    Schema      = jsx:decode(?SCHEMA, [return_maps]),
    FixedSchema = enumerate_discriminator_children(Schema),
    ?assertEqual(lists:sort([<<"Dog">>, <<"Cat">>, <<"WildMix">>]), get_enum(<<"Pet">>, <<"petType">>, FixedSchema)),
    ?assertEqual([<<"WildMix">>], get_enum(<<"Person">>,  <<"personType">>, FixedSchema)),
    ?assertEqual([],              get_enum(<<"Dummy">>,   <<"dummyType">>,  FixedSchema)).

-spec get_test() -> _.
get_test() ->
    ?assertEqual(
       enumerate_discriminator_children(get_raw()),
       ?MODULE:get()
    ).
-endif.
